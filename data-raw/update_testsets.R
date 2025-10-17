library(ojodb)
library(tidyr)

oscn_count_as_filed <- ojo_tbl("count") |>
  distinct(count_as_filed) |>
  collect() |>
  rename(
    description = count_as_filed
  ) |>
  mutate(
    source = "oscn"
  )

oscn_count_as_disposed <- ojo_tbl("count") |>
  distinct(count_as_disposed) |>
  collect() |>
  rename(
    description = count_as_disposed
  ) |>
  mutate(
    source = "oscn"
  )

ocdc_charges <- ojo_tbl("charges", schema = "ocdc_new") |>
  distinct(charge_description) |>
  collect() |>
  rename(
    description = charge_description
  ) |>
  mutate(
    source = "ocdc"
  )

ocdc_charges_history <- ojo_tbl("charges_history", schema = "ocdc_new") |>
  distinct(charge_description) |>
  collect() |>
  rename(
    description = charge_description
  ) |>
  mutate(
    source = "ocdc"
  )

odoc_offense <- ojo_tbl("offense", schema = "odoc") |>
  distinct(description) |>
  collect() |>
  rename(
    description = description
  ) |>
  mutate(
    source = "odoc"
  )

test_data <- bind_rows(
  oscn_count_as_filed,
  oscn_count_as_disposed,
  ocdc_charges,
  ocdc_charges_history,
  odoc_offense
) |>
  distinct() |>
  filter(!is.na(description)) |>
  mutate(
    is_present = TRUE
  ) |>
  pivot_wider(
    names_from = source,
    values_from = is_present,
    values_fill = FALSE
  ) |>
  arrange(
    description
  )

save(test_data, file = here::here("tests", "testthat", "test_data.rda"))
