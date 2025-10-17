# Update ojoRegex datasets to match the Big Ol Spreadsheet,
# saving the data to the package data folder.

library(googlesheets4)
library(dplyr)
library(usethis)
library(devtools)

devtools::load_all()

# Authenticate with Google
googlesheets4::gs4_auth()

# Read the regex flags and categories from the Big Ol Spreadsheet
ojo_regex_flags <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing",
  sheet = "Regex Flag List"
)
ojo_regex_cats <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing",
  sheet = "Clean Categories List",
  col_types = "lcccccccccccccccll"
) |>
  dplyr::filter(in_ojoregex == TRUE) |>
  ojo_add_controlling_charges()

# Save the regex data to the package data
usethis::use_data(ojo_regex_flags, overwrite = TRUE)
usethis::use_data(ojo_regex_cats, overwrite = TRUE)
