library(tidyr)
library(magrittr)
library(dplyr)

# Create input/output directories (.gitignored)
dir.create('input', showWarnings = F)
dir.create('output', showWarnings = F)

# For testing purposes create a client with a birth date of Jan 1, 2020
# that receives each vaccine in the reference file, one per week
day0 = as.Date('2020-01-01')

# Use the same format for immunization history as PEAR with a repeater container
received_agents = readxl::read_xlsx("vaccine_reference.xlsx") |>
  select(Vaccine) |>
  mutate(
    `Date Received` = format(day0 + lubridate::weeks(row_number()), "%b %d, %Y"),
    `Combined` = paste(`Date Received`, Vaccine, sep = ' - ')) |>
  summarize(`Received Agents` = paste(`Combined`, collapse = ', ')) |>
  use_series(`Received Agents`)

pear_like = tibble(
  `Client ID` = '0123456789',
  `Date of Birth` = day0,
  `Received Agents` = received_agents)

writexl::write_xlsx(pear_like, "input/test.xlsx")

rm(pear_like, received_agents, day0)

# Run the usual make_charts script with default parameters
source("make_charts.R")