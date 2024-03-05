devtools::load_all()
library(ojodb)
library(tidyverse)

# ds <- ojo_crim_cases(
#   districts = "all",
#   case_types = c("CF", "CM"),
#   file_years = 2015:2023
# ) |>
#   ojo_collect()

# write_rds(ds, "./data/test-data.rds")

ds <- read_rds("./data/test-data.rds")

# Using new regex --------------------------------------------------------------

final <- ds |>
  ojoregex::apply_ojo_regex(col_to_clean = "count_as_filed", .keep_flags = TRUE)

# Remaining unclassifieds
remaining_nas <- final |>
  filter(is.na(count_as_filed_clean)) |>
  select(-c(id, district, case_number, case_type, date_filed, date_closed, counts, open_counts, disposition,
            count_as_filed_clean)) |>
  group_by(count_as_filed) |>
  mutate(n = n()) |>
  distinct(count_as_filed, .keep_all = TRUE) |>
  select(count_as_filed, n, everything()) |>
  arrange(desc(n))

# Classification breakdown
final |>
  count(count_as_filed_clean, sort = T) |>
  janitor::adorn_percentages("col") |>
  janitor::adorn_totals() |>
  janitor::adorn_pct_formatting() |>
  janitor::adorn_ns()

# All larceny types
final |>
  filter(str_detect(count_as_filed_clean, "(?i)larc")) |>
  count(count_as_filed_clean, sort = T)

# By case type
final |>
  filter(str_detect(count_as_filed_clean, "Larceny")) |>
  count(count_as_filed_clean,
        case_type,
        year = year(date_filed),
        sort = T) |>
  ggplot(aes(x = year,
             y = n,
             fill = count_as_filed_clean)) +
  geom_col() +
  facet_wrap(~case_type)

# By district
final |>
  filter(str_detect(count_as_filed_clean, "Larceny")) |>
  count(count_as_filed_clean,
        district,
        case_type,
        year = year(date_filed),
        sort = T) |>
  ggplot(aes(x = year,
             y = n,
             fill = count_as_filed_clean)) +
  geom_col() +
facet_wrap(~district, scales = "free_y")

