library(readr)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(cli)

col_types_apply_regex <- "cclllcccccclld"

old_data <- read_csv(
  here("tests/testthat/_snaps/ojo_apply_regex/categorized_test_data.csv"),
  col_types = col_types_apply_regex
)

new_data <- read_csv(
  here("tests/testthat/_snaps/ojo_apply_regex/categorized_test_data.new.csv"),
  col_types = col_types_apply_regex
)

diff_data <- full_join(
  old_data,
  new_data,
  by = "description",
  suffix = c(".old", ".new"),
  keep = TRUE,
  relationship = "many-to-many"
)

added_rows <- diff_data |>
  filter(
    is.na(description.old)
  )

removed_rows <- diff_data |>
  filter(
    is.na(description.new)
  )

common_rows <- diff_data |>
  filter(
    !is.na(description.old) & !is.na(description.new)
  )

n_added <- nrow(added_rows)
n_removed <- nrow(removed_rows)
n_common <- nrow(common_rows)

changed_common <- common_rows |>
  filter(
    !identical(category.old, category.new),
    !(is.na(category.old) & is.na(category.new))
  )

transition_data <- changed_common |>
  summarise(
    .by = c(category.old, category.new),
    count = n()
  ) |>
  arrange(desc(count))

changed_data <- transition_data |>
  filter(
    category.old != category.new
  )

cli_div()
cli_h1("{.fn ojo_apply_regex} diff")
cli_end()
cli_text("")
cli_bullets(c(
  "i" = paste("Common Rows:", n_common),
  "v" = paste("Added Rows: ", n_added),
  "x" = paste("Removed Rows:", n_removed)
))
cli_div(
  theme = list(
    .strong = list(
      color = "green"
    ),
    .blue = list(
      color = "blue"
    )
  )
)
cli_h2("Category Changes")
pwalk(
  changed_data,
  \(category.old, category.new, count, ...) {
    cli_li("{.blue {category.old}} -> {category.new} {.strong {count}}")
  }
)
cli_end()

changed_common |>
  count(
    description = description.old,
    category.old,
    category.new,
    sort = TRUE
  )

trace_charge_regex <- function(charge_string) {
  # 1. Pre-clean the charge string
  cleaned_charge <- ojoregex::regex_pre_clean(charge_string)

  # 2. Get all flags and their regex patterns
  regex_flags <- ojoregex::ojo_regex_flags

  # 3. Find which regexes match
  matching_flags <- regex_flags |>
    mutate(
      is_match = stringi::stri_detect(cleaned_charge, regex = paste0("(?i)", regex))
    ) |>
    filter(is_match)

  # 4. Run the full ojo_apply_regex to get the final category
  final_category <- ojo_apply_regex(
    tibble(charge = charge_string),
    col_to_clean = "charge",
    .quiet = TRUE
  )$charge_clean

  match_view <- matching_flags |>
    mutate(
      match = map_chr(
        regex,
        \(regex) {
          v <- str_view(cleaned_charge, paste0("(?i)", regex)) |>
            as.character()

          ifelse(
            length(v) == 0,
            NA_character_,
            v
          )
        }
      )
    ) |>
    select(
      flag, regex, match
    )

  pwalk(
    match_view,
    \(flag, regex, match) {
      cli::cli_text("{flag}: {.val {regex}}")
      cli::cli_li("{match}")
    }
  )

  # 5. Return a structured list with the results
  list(
    original_charge = charge_string,
    cleaned_charge = cleaned_charge,
    final_category = final_category,
    matching_flags = matching_flags |> select(flag, regex)
    # Potentially, you could even try to programmatically extract and show
    # the specific case_when condition that was met.
  )
}

trace_charge_regex("FLIGHT TO AVOID - COLLIN CO TEXAS")
