# Don't warn about package conflicts
options(conflicts.policy = list("warn" = F))

library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(magrittr)
library(kableExtra)

# Latex utility functions
source("latex_utilities.R")

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
        rep("text", 4),
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
      \(x) if_else(x == "RR N/A", NA_character_, x)))

nested_clients = clients |>
  nest_by(`School`, .keep = T) |>
  use_series(data)

for(i in seq_along(nested_clients)){
nested_clients[[i]] |>
  select(`Last Name`, `First Name`, `Date of Birth`) |>
  arrange(`Last Name`, `Date of Birth`) |>
  
}

for(i in seq_along(nested_clients)){
  # Initialize list
  list_data = list()
  
  # Get school name
  list_data$school = nested_clients[[i]] |> use_series(`School`) |> extract(1)

  # Generate table content
  list_data$list_data = nested_clients[[i]] |>
    select(`Last Name`, `First Name`, `Date of Birth`) |>
    
    # Sort
    arrange(`Last Name`, `Date of Birth`)

  # Generate LaTeX table content
  list_data$list_LaTeX = list_data$list_data |>
    # Make into LaTeX code
    kableExtra::kable("latex") |>
    
    # Trim header and footer (header and footer defined directly in Rmd)
    LaTeX_trim_lines(5L, 2L)
  
  readr::write_csv(
    x = list_data$list_data, 
    file = paste0("output/DATA FILE ", list_data$school, " Overdue Immunization Letter Distribution List.csv"))
  
  rmarkdown::render(
    input = "distribution_list_template.Rmd",
    output_file = paste(list_data$school, "Overdue Immunization Letter Distribution List.pdf"),
    output_dir = "output",
    params = list(data = list_data),
    envir = new.env(),
    runtime = "static",
    quiet = T)
}