devtools::load_all()

library(readr)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(cli)
library(stringr)
library(googlesheets4)

col_types_apply_regex <- "cclllcccccclld"

data <- read_csv(
  here("tests/testthat/_snaps/ojo_apply_regex/categorized_test_data.csv"),
  col_types = col_types_apply_regex
)

View(data)
