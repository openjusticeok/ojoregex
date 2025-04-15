#' Add controlling charges to the dataset
#'
#' This function processes a dataset of charges and adds columns to classify
#' the maximum sentence types and calculate control ranks based on the severity
#' of the sentences.
#'
#' @param ojo_regex_cats A data frame containing charge information, including
#'   columns `max_sentence_first_offense` and `max_sentence_any`.
#'
#' @return A data frame with additional columns for sentence classifications
#'   and control ranks.
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
      control_rank = dplyr::case_when(
        max_sentence_first_offense_type == "Death" ~ 1000,
        max_sentence_first_offense_type == "Life" ~ 100,
        max_sentence_first_offense_type == "Time in DOC" ~ (max_sentence_first_time_in_doc / lubridate::years(1)) |> round(2),
        max_sentence_first_offense_type == "Fine" ~
          (1 / readr::parse_number(max_sentence_any, na = c("Life", "Death", "Other"))) |> round(2),
        max_sentence_first_offense_type == "Other / Unknown" ~ 0,
        TRUE ~ NA
      ),
      # Tie breakers
      control_rank = dplyr::case_when(
        str_detect(clean_charge_description, "(?i)murder|manslau|homicid") ~ control_rank + 0.5,
        str_detect(clean_charge_description, "(?i)rape|sex|molest") ~ control_rank + 0.4,
        str_detect(clean_charge_description, "(?i)child") ~ control_rank + 0.3,
        str_detect(clean_charge_description, "(?i)aggrivated|armed|weapon") ~ control_rank + 0.2,
        TRUE ~ control_rank
      )
    )|>
    dplyr::select(
      -max_sentence_first_time_in_doc,
      -max_sentence_any_time_in_doc,
      -max_sentence_first_offense_type,
      -max_sentence_any_offense_type
    )

  return(result)

}
