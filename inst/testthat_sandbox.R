devtools::load_all()
library(ojodb)

ds <- ojo_crim_cases(
  districts = "Oklahoma",
  case_types = c("CM", "CF"),
  file_years = 2023:2023
) |>
  ojo_collect()

# Using old methods ------------------------------------------------------------
# ds |>
#   mutate(sex_work_related = str_detect(count_as_filed, "(?i)prost|prositu")) |>
#   filter(sex_work_related) |>
#   nrow()


# Using new regex --------------------------------------------------------------
final <- ds |>
  ojoregex::apply_ojo_regex(col_to_clean = "count_as_filed")

final |>
  filter(str_detect(count_as_filed_clean, "Sex Work")) |>
  count(count_as_filed_clean, sort = T) |>
  janitor::adorn_totals()

final |>
  filter(str_detect(count_as_filed_clean, "Sex Work")) |>
  count(district, sort = T)
