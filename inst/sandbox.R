devtools::load_all()
library(ojodb)
library(tidyverse)
library(tictoc)

# ds <- ojo_crim_cases(
#   districts = "all",
#   case_types = c("CF", "CM"),
#   file_years = 2022:2022,
# ) |>
#   ojo_collect()

# write_rds(ds, "/mnt/data/data/test-data-all.rds")
#
# beepr::beep()

ds <- read_rds("/mnt/data/data/test-data-tr-all.rds")

# Using new regex --------------------------------------------------------------
tic()
final <- ds |>
  head(10000) |>
  ojoregex::ojo_apply_regex(col_to_clean = "count_as_filed",
                            .keep_flags = FALSE, .include_cats = TRUE)
toc()

# Percent categorized:
cli::cli_alert_success(
  paste0(100 * round((final |> filter(!is.na(count_as_filed_clean)) |> nrow()) / nrow(final |> filter(!is.na(count_as_filed))), 4), "% Done!!")
)
beepr::beep()

# CDS (Other / Unspecified) refinement for LOFT stuff --------------------------
final |>
  filter(
    count_as_filed_clean == "CDS (Other / Unspecified)",
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "manufacture")), # no "Manufacturing"
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "intoxication")), # no DWI or public intox
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "under_the_influence")),
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "forge")), # no "obtain by fraud"
    !str_detect(count_as_filed, "(?i)telecom|wire"), # No "use of telecom device in cds transaction"
    !str_detect(count_as_filed, "(?i)cult"), # no "cultivation of cds"
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "deliver")), # no "delivery of cds"
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "distribution")), # no "distribution of cds"
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "phone")), # no "cellular phone"
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "dui_or_apc")),
    !str_detect(count_as_filed, ojo_get_flag_regex(flag = "child")),
    !str_detect(count_as_filed, "(?i)traff") # no "trafficking"
  ) |>
  count(count_as_filed, sort = T) |>
  view()

# Larceny (Other / Unspecified) refinement for LOFT stuff ----------------------


# Fraud (Other / Unspecified) refinement for LOFT stuff ------------------------


# # Classifications rundown
# final |>
#   # filter(str_detect(count_as_filed_clean, "Fraud")) |>
#   group_by(count_as_filed_clean) |>
#   summarize(
#     n = n(),
#     # all_counts = paste(count_as_filed, collapse = "; ")
#   ) |>
#   arrange(desc(n)) |>
#   print(n = 20)

explore <- final |>
  # filter(burgle) |>
  count(count_as_filed, count_as_filed_clean, sort = T)

# Remaining unclassified
remaining_nas <- final |>
  filter(is.na(count_as_filed_clean),
         !is.na(count_as_filed)) |>
  select(-c(id, district, case_number, case_type, date_filed, date_closed, open_counts, disposition,
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
  # filter(str_detect(count_as_filed_clean, "Obstruction")) |>
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


# --- temp ---
# larceny_cases <- final |>
#   group_by(case = paste(case_number, district)) |>
#   # filter(any(str_detect(count_as_filed_clean, "Larceny"))) |>
#   summarize(
#     n_charges = n(),
#     list_charges = paste(count_as_filed, collapse = "; "),
#     list_clean_charges = paste(count_as_filed_clean, collapse = "; "),
#     has_larceny = any(count_as_filed_clean %in% c("Larceny (Shoplifting)", "Larceny (Grand)", "Larceny (Petit)",
#                                                   "Larceny (Other / Unspecified)", "Larceny (Auto)")),
#     has_rcsp = any(count_as_filed_clean == "Receiving / Concealing Stolen Property")
#   )
#
# n_larc_cases <- larceny_cases |>
#   filter(has_larceny) |>
#   nrow()
#
# n_w_rcsp <- larceny_cases |>
#   filter(has_larceny, has_rcsp) |>
#   nrow()
#
# n_w_rcsp / n_larc_cases
#
#
# larceny_cases <- final |>
#   group_by(case = paste(case_number, district)) |>
#   # filter(any(str_detect(count_as_filed_clean, "Larceny"))) |>
#   mutate(
#     n_charges = n(),
#     # list_charges = paste(count_as_filed, collapse = "; "),
#     # list_clean_charges = paste(count_as_filed_clean, collapse = "; "),
#     has_larceny = any(count_as_filed_clean %in% c("Larceny (Shoplifting)", "Larceny (Grand)", "Larceny (Petit)",
#                                                   "Larceny (Other / Unspecified)", "Larceny (Auto)")),
#     has_rcsp = any(count_as_filed_clean == "Receiving / Concealing Stolen Property")
#   ) |>
#   filter(has_larceny)
#
# larceny_cases |>
#   ungroup() |>
#   count(count_as_filed_clean, sort = T)
