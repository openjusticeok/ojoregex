#' Update ojoRegex datasets to match the Big Ol Spreadsheet
#'
#' This function reads the Big Ol Spreadsheet and saves the data to the package data folder.
#'
#' @param email The email address to use for Google authentication.
#'
#' @return A cleaned and categorized dataset with charge descriptions in the specified column, along with any additional columns present in the original dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update_datasets("abell@okpolicy.org")
#'}
update_datasets <- function(email = NULL) {
  # Authenticate with Google
  googlesheets4::gs4_auth(email = email)

  # Read the regex flags and categories from the Big Ol Spreadsheet
  ojo_regex_flags <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing",
                                               sheet = "Regex Flag List")
  ojo_regex_cats <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing",
                                              sheet = "Clean Categories List",
                                              col_types = "lcccccccccccccll") |>
    dplyr::filter(in_ojoregex == TRUE)

  # Save the regex data to the package data
  save(ojo_regex_flags, file = here::here("data", "ojo_regex_flags.rda"))
  save(ojo_regex_cats, file = here::here("data", "ojo_regex_cats.rda"))

  cli::cli_alert_success("Regex flags and categories updated!")
}

