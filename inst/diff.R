devtools::load_all()

library(readr)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(cli)
library(stringr)

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

get_changes <- function(data, variable) {
  # These are STRINGS: e.g., "race.old", "race.new"
  old_variable <- paste0(variable, ".old")
  new_variable <- paste0(variable, ".new")

  changed_common <- data |>
    filter(
      !identical(.data[[old_variable]], .data[[new_variable]]),
      !(is.na(.data[[old_variable]]) & is.na(.data[[new_variable]]))
    )

  transition_data <- changed_common |>
    summarise(
      # FIX 2: Use all_of() to be explicit with the character vector
      .by = all_of(c(old_variable, new_variable)),
      count = n()
    ) |>
    arrange(desc(count))

  changed_summary <- transition_data |>
    filter(
      .data[[old_variable]] != .data[[new_variable]]
    )

  changed_data <- changed_summary |>
    pmap(
      \(...) {
        .row_data <- list(...) 

        .current_old_val <- .row_data[[old_variable]]
        .current_new_val <- .row_data[[new_variable]]

        changed_common |>
          filter(
            .data[[old_variable]] == .current_old_val,
            .data[[new_variable]] == .current_new_val
          )
      }
    )

  cli({
    cli_div()
    cli_h1("{.fn ojo_apply_regex} diff")
    cli_end()
    cli_text("")
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
    cli_h2("{.val {variable}} Changes")
    pwalk(
      changed_summary,
      \(...) {
        .row_data <- list(...)
        # FIX 1: Wrap .row_data[...] in parentheses to avoid cli error
        cli_li("{.blue {(.row_data[[old_variable]])}} -> {(.row_data[[new_variable]])} {.strong {(.row_data$count)}}")
      }
    )
    cli_end()
  })

  invisible(
    list(
      changed_summary,
      changed_data
    )
  )
}

get_changes(diff_data, "category")
get_changes(diff_data, "subcategory")
get_changes(diff_data, "description_clean")

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

changed_summary <- transition_data |>
  filter(
    category.old != category.new
  )

cli({
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
    changed_summary,
    \(category.old, category.new, count, ...) {
      cli_li("{.blue {category.old}} -> {category.new} {.strong {count}}")
    }
  )
  cli_end()
})

changed_data <- changed_summary |>
  pmap(
    \(category.old, category.new, ...) {
      common_rows |>
        filter(
          category.old == !!category.old,
          category.new == !!category.new
        )
    }
  )

all(changed_summary$count == changed_data |>
  map_int(nrow))

changed_data |>
  map(\(x) x |> distinct(description.old))

trace_charge_regex <- function(charge_string) {
  cleaned_charge <- regex_pre_clean(charge_string)

  regex_flags <- ojo_regex_flags

  matching_flags <- regex_flags |>
    mutate(
      is_match = stringi::stri_detect(cleaned_charge, regex = paste0("(?i)", regex))
    ) |>
    filter(is_match)

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

  cli::cli_text("{.strong Matching Flags}")
  pwalk(
    match_view,
    \(flag, regex, match) {
      cli::cli_rule()
      cli::cli_text("{flag}: {.val {regex}}")
      cli::cli_li("{match}")
    }
  )

  res <- list(
    original_charge = charge_string,
    cleaned_charge = cleaned_charge,
    final_category = final_category,
    matching_flags = matching_flags |> select(flag, regex)
  )

  invisible(res)
}

trace_charge_regex(changed_data[[1]][[1, 1]])
