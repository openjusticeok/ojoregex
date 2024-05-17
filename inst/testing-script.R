library(ojodb)
library(tidyverse)
library(googlesheets4)
devtools::load_all()

# Test data ====================================================================
# test_data <- ojo_crim_cases(districts = "all",
#                             case_types = c("CF", "CM", "TR"),
#                             file_years = c(2018:2024)) |>
#   filter(!is.na(count_as_filed)) |>
#   count(count_as_filed, sort = T) |>
#   ojo_collect()
#
# readr::write_csv(test_data, here::here("data", "test_data.csv"))
test_data <- readr::read_csv(here::here("data", "test_data.csv"))

test_data <- ojo_crim_cases(districts = "all",
                            case_types = c("CF", "CM", "TR"),
                            file_years = c(2018:2024)) |>
  ojo_collect()

test_data_cf_cm <- test_data |>
  filter(case_type != "TR") |>
  select(-c(id, counts, open_counts))

ds <- test_data_cf_cm |>
  ojoregex::apply_ojo_regex(
    col_to_clean = "count_as_filed",
    .keep_flags = FALSE
  )

ds |>
  count(count_as_filed, count_as_filed_clean, sort = T) |>
