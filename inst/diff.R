library(readr)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(cli)

old_data <- read_csv(here("inst/new.csv"))
new_data <- read_csv(here("inst/old.csv"))

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
	keep = TRUE
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
  theme = list (
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

