##############
# Parameters #
##############

# Diseases, in order, to include immunization history chart
# Mark as 'F' to collapse the disease into the 'Other' column
chart_diseases = c(
  'Diphtheria'    = T,
  'Tetanus'       = T,
  'Pertussis'     = T,
  'Polio'         = T,
  'Hib'           = T,
  'Pneumococcal'  = T,
  'Rotavirus'     = T,
  'Measles'       = T,
  'Mumps'         = T,
  'Rubella'       = T,
  'Meningococcal' = T,
  'Varicella'     = T,
  'Hepatitis B'   = T,
  'HPV'           = T
  )

# Vaccines or agents to ignore in/drop from immunization history
ignore_agents = c(
  'RSVAb',
  'VarIg',
  'HBIg',
  'RabIg',
  'Ig')

# Used to calculate student age at time of mail delivery
# Students 16 and older can be addressed directly
# Letters for students under 16 should be addressed to their parent/guardian
delivery_date = as.Date('2024-11-15')

# Minimum number of rows to show in immunization history chart
# Charts will be padded with rows as appropriate
min_rows = 5L

##################
# End parameters #
##################

# Don't warn about package conflicts
options(conflicts.policy = list("warn" = F))

library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(magrittr)
library(kableExtra)

# Load vaccine - disease reference table
# Collapse as desired
vax_ref = readxl::read_xlsx(
  "vaccine_reference.xlsx",
  col_types = c("text", rep("logical", 15))) |>
  rowwise() |>
  mutate(`Other` = any(
    c_across(
      all_of(c(
        names(chart_diseases[chart_diseases == F]),
        "Other"))))) |>
  ungroup() |> 
  select(all_of(c(
    "Vaccine",
    names(chart_diseases[chart_diseases == T]),
    "Other")))

chart_num_diseases = sum(chart_diseases)

# Format column header information for LaTeX
chart_col_header = c(names(chart_diseases[chart_diseases == T]), "Other") |> 
  str_replace_all(
    pattern = "^([\\w\\s]+)$",
    replacement = "\\\\rotatebox{90}{\\1}")

chart_col_header = c("Date Given", "At Age", chart_col_header, "Vaccine(s)") |>
  paste(collapse = " & ")

# Vaccination history string parser
parse_vaccination_history = function(x, ignore_agents = NULL) {
  if (is.na(x) || nchar(x) == 0) {
    return(tibble(`Date Given` = as.Date(character(0)), `Vaccine` = character(0)))
  }
  
  x |>
    # Delete trailing comma
    str_remove(pattern = ",$") |>
    
    # Consistent date formatting can help to precisely split string
    str_split(pattern = ",\\s*(?=\\w{3,3} \\d{1,2}, \\d{4,4})") |>
    extract2(1) |>
    str_split(pattern = "(?<=^\\w{3,3} \\d{1,2}, \\d{4,4}) - ") |>
    
    # Format date - vaccine string pairs into table, and format as date
    map(\(x) tibble(
      `Date Given` = as.Date(x[1], format = "%b %d, %Y"),
      `Vaccine` = x[2])) |>
    list_rbind() |>
    filter(!(`Vaccine` %in% ignore_agents))
}


#Y M age formatting
diff_ym = function(date1, date2){
  ym_paste = function(x){paste0(floor(x / 12), "Y ", floor(x %% 12), "M")}
  lubridate::time_length(date1 - date2, unit = "month") |>
  ym_paste()
  }


# Latex utility functions
source("latex_utilities.R")

# Create a vector which will track vaccine occurrences
vaccine_occurrences = character(0)

# List XLSX files in directory
clients = list.files(
  path = "input/",
  pattern = ".xlsx$",
  full.names =  T) |>
  
  # Ensure files listed in ascending date order
  sort() |>
    
  # Read all listed XLSX files
  # Column names and types specific to the report created
  purrr::map(\(x) {
    readxl::read_xlsx(
      path = x,
      col_types = c(
        rep("text", 7),
        rep("date", 1),
        rep("text", 8))) |>
    select(
      `School` = `School Name`,
      `Client ID` = `Client Id`,
      `First Name`,
      `Last Name`,
      `Date of Birth`,
      `Street Address Line 1`,
      `Street Address Line 2`,
      `City`,
      `Province` = `Province/Territory`,
      `Postal Code`,
      `Vaccines Due` = `Overdue Disease`,
      `Received Agents` = `Imms Given`
      )
    }) |>

  # Bind list of data frames
  bind_rows() |>
  
  # Make text uppercase
  mutate(
    across(
    .cols = all_of(c(
      "School", "First Name", "Last Name",
      "Street Address Line 1", "Street Address Line 2",
      "City", "Province", "Postal Code")),
    .fns = \(x){
      x |>
        str_to_upper() |>
        str_squish() |>
        LaTeX_escape()
      })) |>
  
  mutate(
    # Formatting of fields
    across(`Date of Birth`,
      \(x) as.Date(x)),

    across(`Street Address Line 2`,
      \(x) if_else(x == "RR N/A", NA_character_, x)),

    across(`Vaccines Due`,
      \(x) {x |>
        # Remove trailing commas
        str_remove_all(
         pattern = ",$") |>
          
        # Refer to diseases by common names
        str_replace_all(
         pattern = c(
          "Poliomyelitis" = "Polio",
          "Human papilloma virus infection" = "Human Papillomavirus (HPV)",
          "Varicella" = "Varicella (Chickenpox)")) |>
          
        # Create LaTeX list of due immunizations
        str_replace_all(
         pattern = ", ",
          replacement = "\n \\\\item ")}),
 
    across(`Received Agents`,
      \(x) if_else(str_detect(x, pattern = "^- ,$"), NA_character_, x)),

    # Create `Received Agents Table` based on string of vaccinations
    `Received Agents Table` = map2(
      .x = `Received Agents`, 
      .y = `Date of Birth`,
      .f = \(x, y){
        z = parse_vaccination_history(x, ignore_agents = ignore_agents)
        vaccine_occurrences <<- c(vaccine_occurrences, use_series(z, Vaccine))
        
        z |>
          mutate(`At Age` = diff_ym(`Date Given`, y)) |>
          arrange(`Date Given`)
        }),
    
    # Create vaccination history chart based on `Received Agents Table`
    `Vaccine History Table` = map(
      .x = `Received Agents Table`,
      .f = \(x){x = x |>
        # Indicators for protection for diseases
        left_join(vax_ref, by = c("Vaccine"), relationship = "many-to-one") |>
    
        # Group vaccines given on same day
        group_by(`Date Given`, `At Age`) |>
        summarize(
          across(
            .cols = where(is.logical),
            .fns = \(x) any(x)),
          `Vaccine(s)` = paste(Vaccine, collapse = ", "),
          .groups = "drop") |>
        
        # Substitute "unsp" for "unspecified" in vaccines column to save space
        mutate(
          `Vaccine(s)` = str_replace_all(
            `Vaccine(s)`,
            pattern = "unspecified",
            replacement = "unsp"))}),
    
    # Calculate number of rows in vaccine history chart
    # May be used to separate out particularly long vaccine histories which
    # exceed two page limit
    `Vaccine History Table Rows` = map(
      .x = `Vaccine History Table`,
      .f = \(x){x = x |> nrow()}),

    # Create a LaTeX table
    `Vaccine History LaTeX` = map(
      .x = `Vaccine History Table`,
      .f = \(x){x = x |>
          
        # Use circle symbol for True, blank for false
        mutate(
          across(
          .cols = where(is.logical),
          .fns = \(x) if_else(x, "\\mycircle", "", missing = "")))  |>
          
        # Create LaTeX code
        kable("latex", escape = FALSE) |>
          
        # Trim header and footer (header and footer defined directly in Rmd)
        LaTeX_trim_lines(5L, 1L) |>
        
        # Pad table with empty rows in LaTeX
        # Number of diseases + Date Given, At Age, Other, And Vaccine(s)
        LaTeX_pad_rows(min_rows, chart_num_diseases + 4L)
      })
    )

vaccine_occurrences_table = tibble(`Vaccine` = vaccine_occurrences) |>
  group_by(Vaccine) |>
  summarize(n = n(), .groups = "drop") |>
  full_join(
    select(vax_ref, Vaccine),
    by = join_by(Vaccine),
    relationship = "one-to-one",
    keep = T) |>
  rename(c(
    "Vaccine" = "Vaccine.x",
    "Matched" = "Vaccine.y")) |>
  mutate(Matched = !is.na(Matched)) |>
  arrange(Vaccine)

readr::write_csv(
  vaccine_occurrences_table,
  "output/vaccine_occurrences.csv")

if(filter(vaccine_occurrences_table, Matched == F) |> dim() |> extract(1) > 0L){
  warning("Unmatched vaccines detected. Review output/vaccine_occurrences.csv,
       and either make additions to ignore_agents parameter, or to
       vaccine reference file (vaccine_reference.xlsx) and re-run.") 
  }

rm(vaccine_occurrences)

# Additional data processing for notice
clients = clients |>
  
   # Final formatting and variable selection for document creation
  rowwise() |>
  mutate(
    `Name` = paste(na.omit(c_across(ends_with("Name"))), collapse = " ")) |>
  ungroup() |>
  mutate(
    # Non-breaking spaces within name
    `Name` = str_replace_all(`Name`, "\\s", '~'),
    `Birth Year`= lubridate::year(`Date of Birth`),
    `Age 16+` = lubridate::time_length(
      delivery_date - `Date of Birth`,
      unit = "year") >= 16,
    `Date of Birth` = format(`Date of Birth`, "%B %d, %Y"))

# Batch clients for PDF generation
clients = clients |>
  nest_by(`School`, .keep = T) |>
  use_series(data) |>
  map(\(x) {
    x |>
    nest_by(`Birth Year`, .keep = T) |>
    use_series(data)
  })

# For producing all notices:
for (iter_facility in seq_along(clients)) {
  for (iter_batch in seq_along(clients[[iter_facility]])) {
    notice_data = clients[[iter_facility]][[iter_batch]]
    notice_filename = paste0(
      str_replace_all(notice_data$`School`[1], "\\W+", "_"),
      "_",
      notice_data$`Birth Year`[1],
      ".pdf")
    
    cat("Building:", notice_filename, "\n")

    rmarkdown::render(
      input = "chart_template.Rmd",
      output_file = notice_filename,
      output_dir = "output",
      params = list(
        client_data = notice_data,
        chart_num_diseases = chart_num_diseases,
        chart_col_header = chart_col_header),
      quiet = T)
    
    rm(notice_data)
    rm(notice_filename)
  }
}