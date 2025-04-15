#'
ojo_add_controlling_charges <- function(ojo_regex_cats) {

  result <- ojo_regex_cats |>
    dplyr::mutate(
      max_sentence_first_offense_type = dplyr::case_when(
        stringr::str_detect(max_sentence_first_offense, "(?i)death") ~ "Death",
        stringr::str_detect(max_sentence_first_offense, "(?i)life") ~ "Life",
        stringr::str_detect(max_sentence_first_offense, "(?i)year") ~ "Time in DOC",
        stringr::str_detect(max_sentence_first_offense, "(?i)month") ~ "Time in DOC",
        stringr::str_detect(max_sentence_first_offense, "(?i)week") ~ "Time in DOC",
        stringr::str_detect(max_sentence_first_offense, "(?i)day") ~ "Time in DOC",
        stringr::str_detect(max_sentence_first_offense, "(?i)\\$") ~ "Fine",
        TRUE ~ "Other / Unknown"
      ),
      max_sentence_any_offense_type = dplyr::case_when(
        stringr::str_detect(max_sentence_any, "(?i)death") ~ "Death",
        stringr::str_detect(max_sentence_any, "(?i)life") ~ "Life",
        stringr::str_detect(max_sentence_any, "(?i)year") ~ "Time in DOC",
        stringr::str_detect(max_sentence_any, "(?i)month") ~ "Time in DOC",
        stringr::str_detect(max_sentence_any, "(?i)week") ~ "Time in DOC",
        stringr::str_detect(max_sentence_any, "(?i)day") ~ "Time in DOC",
        stringr::str_detect(max_sentence_any, "(?i)\\$") ~ "Fine",
        TRUE ~ "Other / Unknown"
      ),
      max_sentence_first_time_in_doc = dplyr::if_else(
        max_sentence_first_offense_type == "Time in DOC",
        lubridate::as.period(max_sentence_first_offense),
        NA
      ),
      max_sentence_any_time_in_doc = dplyr::if_else(
        max_sentence_any_offense_type == "Time in DOC",
        lubridate::as.period(max_sentence_any),
        NA
      ),
      control_rank_first = dplyr::case_when(
        max_sentence_first_offense_type == "Death" ~ 1000,
        max_sentence_first_offense_type == "Life" ~ 100,
        max_sentence_first_offense_type == "Time in DOC" ~ max_sentence_first_time_in_doc@year,
        max_sentence_first_offense_type == "Fine" ~
          (1 / readr::parse_number(max_sentence_first_offense, na = c("Life", "Death", "Other"))) |> round(2),
      max_sentence_first_offense_type == "Other / Unknown" ~ 0,
      TRUE ~ NA
      ),
      control_rank_any = dplyr::case_when(
        max_sentence_any_offense_type == "Death" ~ 1000,
        max_sentence_any_offense_type == "Life" ~ 100,
        max_sentence_any_offense_type == "Time in DOC" ~ max_sentence_any_time_in_doc@year,
        max_sentence_any_offense_type == "Fine" ~
          (1 / readr::parse_number(max_sentence_any, na = c("Life", "Death", "Other"))) |> round(2),
        max_sentence_any_offense_type == "Other / Unknown" ~ 0,
        TRUE ~ NA
      ),
      control_rank_refined = dplyr::case_when(
        max_sentence_first_offense_type == "Death" ~ 1000,
        max_sentence_first_offense_type == "Life" ~ 100,
        max_sentence_first_offense_type == "Time in DOC" ~ max_sentence_first_time_in_doc@year,
        max_sentence_first_offense_type == "Fine" ~
          (1 / readr::parse_number(max_sentence_any, na = c("Life", "Death", "Other"))) |> round(2),
        max_sentence_first_offense_type == "Other / Unknown" ~ 0,
        TRUE ~ NA
      ),
      # Tie breakers
      control_rank_refined = dplyr::case_when(
        str_detect(clean_charge_description, "(?i)murder|manslau") ~ control_rank_refined + 0.5,
        str_detect(clean_charge_description, "(?i)rape|sex") ~ control_rank_refined + 0.4,
        str_detect(clean_charge_description, "(?i)child") ~ control_rank_refined + 0.3,
        str_detect(clean_charge_description, "(?i)aggrivated|armed|weapon") ~ control_rank_refined + 0.2,
        TRUE ~ control_rank_refined
      )
    )

}

# Testing
refined <- result |>
  count(clean_charge_description, control_rank_refined) |>
  arrange(desc(control_rank_refined)) |>
  select(-n)
first <- result |>
  count(clean_charge_description, control_rank_first) |>
  arrange(desc(control_rank_first)) |>
  select(-n)
any <- result |>
  count(clean_charge_description, control_rank_any) |>
  arrange(desc(control_rank_any)) |>
  select(-n)

