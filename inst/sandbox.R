devtools::load_all()
library(ojodb)
library(tidyverse)
library(tictoc)

# ds <- ojo_crim_cases(
#   districts = "all",
#   case_types = c("CF", "CM", "TR"),
#   file_years = 2000:2024,
# ) |>
#   ojo_collect()
#
# write_rds(ds, "./data/test-data-all.rds")
#
# beepr::beep()

# ds <- read_rds("./data/test-data-tr-all.rds")
ds <- read_rds("./data/test-data-all.rds")

# Using new regex --------------------------------------------------------------
tic()
final <- ds |>
  ojoregex::apply_ojo_regex(col_to_clean = "count_as_filed", .keep_flags = TRUE, .update_cache = TRUE)
toc()

# Percent categorized:
cli::cli_alert_success(
  paste0(100 * round((final |> filter(!is.na(count_as_filed_clean)) |> nrow()) / nrow(final |> filter(!is.na(count_as_filed))), 4), "% Done!!")
)
beepr::beep()

# Classifications rundown
final |>
  filter(str_detect(count_as_filed_clean, "Fraud")) |>
  group_by(count_as_filed_clean) |>
  summarize(
    n = n(),
    # all_counts = paste(count_as_filed, collapse = "; ")
  ) |>
  arrange(desc(n)) |>
  print(n = 300)


explore <- final |>
  # filter(burgle) |>
  count(count_as_filed, count_as_filed_clean, sort = T)

# Remaining unclassified
remaining_nas <- final |>
  filter(is.na(count_as_filed_clean)) |>
  select(-c(id, district, case_number, case_type, date_filed, date_closed, counts, open_counts, disposition,
            count_as_filed_clean)) |>
  group_by(count_as_filed) |>
  mutate(n = n()) |>
  distinct(count_as_filed, .keep_all = TRUE) |>
  select(count_as_filed, n, everything()) |>
  arrange(desc(n))

remaining_nas |>
  arrange(desc(n)) |>
  select(count_as_filed, n)

#

final |>
  filter(str_detect(count_as_filed_clean, "Obstruction")) |>
  # distinct(count_as_filed, .keep_all = TRUE) |>
  count(count_as_filed, count_as_filed_clean, sort = T) |> view()

final |>
  count(count_as_filed_clean, sort = T) |>
  janitor::adorn_percentages("col") |>
  janitor::adorn_totals() |>
  janitor::adorn_pct_formatting() |>
  janitor::adorn_ns()

final |>
  filter(str_detect(count_as_filed_clean, "(?i)larc")) |>
  count(count_as_filed_clean, sort = T)

final |>
  filter(str_detect(count_as_filed_clean, "Larceny")) |>
  count(count_as_filed_clean,
        # district,
        case_type,
        year = year(date_filed),
        sort = T) |>
  ggplot(aes(x = year,
             y = n,
             fill = count_as_filed_clean)) +
  geom_col() +
  facet_wrap(~case_type)
  # facet_wrap(~district, scales = "free_y")
  # guides(fill = "none")


