library(readr)
library(here)
library(dplyr)
library(tidyr)
library(purrr)

old_data <- read_csv(here("inst/new.csv"))
new_data <- read_csv(here("inst/old.csv"))

diff_data <- full_join(
	old_data,
	new_data,
	by = "unique_record_id",
	suffix = c(".old", ".new"),
	keep = TRUE
)

added_rows <- diff_data |>
	filter(
		is.na(unique_record_id.old)
	)

removed_rows <- diff_data |>
	filter(
		is.na(unique_record_id.new)
	)

common_rows <- diff_data |>
	filter(
		!is.na(unique_record_id.old) & !is.na(unique_record_id.new)
	)

n_added <- nrow(added_rows)
n_removed <- nrow(removed_rows)
n_common <- nrow(common_rows)

changed_common <- common_rows |>
	filter(
		!identical(category.old, category.new)
	)

transition_data <- changed_common |>
	summarise(
		.by = c(category.old, category.new),
		count = n()
	) |>
	arrange(desc(count))

transition_data |>
	filter(
		category.old != category.new
	)

