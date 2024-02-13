#' Apply OJO Regex
#'
#' This function applies regular expressions patterns to clean and categorize charge descriptions in a given dataset.
#'
#' @param data A data frame containing the dataset to be processed.
#' @param col_to_clean The name of the column in the dataset containing the charge descriptions to be cleaned and categorized.
#' @param .keep_flags Logical value indicating whether to keep the concept flags generated during processing. Defaults to FALSE, which returns only the cleaned dataset without the flags.
#'
#' @return A cleaned and categorized dataset with charge descriptions in the specified column, along with any additional columns present in the original dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example dataset
#' data(example_data)
#'
#' # Apply OJO Regex to clean and categorize charge descriptions
#' cleaned_data <- apply_ojo_regex(data = example_data, col_to_clean = "charge_description")
#'}
apply_ojo_regex <- function(data, col_to_clean, .keep_flags = FALSE) {

  # Validate data ==============================================================
  data_names <- names(data)
  clean_col_name <- paste0(col_to_clean, "_clean")

  # Check if col_to_clean exists in the data frame
  if (!col_to_clean %in% colnames(data)) {
    stop("Column not found in data frame.")
  }

  # Regex list (in progress; replace w/ CSV before release):
  googlesheets4::gs4_auth(email = "abell@okpolicy.org")
  regex <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing")

  # Creating a list of groups and their relevant flags also (like cds | meth | paraphernalia ... = any_drugs)
  # these should all start with any_ prefix
  group_data <- regex |>
    dplyr::filter(!is.na(group)) |>
    dplyr::group_by(group) |>
    dplyr::summarize(
      list_flags = paste(flag, collapse = "|")
    )

  # Applying flags ===============================================================
  # Creating function to apply the patterns --------------------------------------
  apply_regex_pattern <- function(data, flag, regex_pattern) {
    data |>
      dplyr::mutate(
        !!flag := stringr::str_detect(!!dplyr::sym(col_to_clean),
                                      stringr::regex(regex_pattern, ignore_case = TRUE))
      )
  }

  # Pre-cleaning steps -----------------------------------------------------------
  flagged_data <- data |>
    dplyr::mutate(
      # This removes "... in concert with _____"
      !!paste0(col_to_clean, "_clean") := ojoregex::regex_pre_clean(!!dplyr::sym(col_to_clean))
    )

  # Apply function over every row of the dataset... ------------------------------
  for(i in seq(nrow(regex))) {
    flagged_data <- apply_regex_pattern(flagged_data,
                                        regex$flag[i],
                                        regex$regex[i])
  }

  # ...then, apply the groups where relevant... ----------------------------------
  for (j in seq(nrow(group_data))) {
    group_flag <- group_data$group[j]
    flags <- unlist(stringr::str_split(group_data$list_flags[j], "\\|"))

    flagged_data <- flagged_data |>
      dplyr::mutate(!!group_flag := rowSums(dplyr::select(flagged_data, all_of(flags)), na.rm = TRUE) > 0)
  }

  # ...now we have the flags in place, and we're ready to categorize!

  # Categorizing =================================================================

  clean_data <- flagged_data |>
    dplyr::mutate(
      # Cleaned charge descriptions (most specific, i.e. "simple possession", "kidnapping", etc.)
      # Later ones should overwrite previous ones, so maybe order by ascending priority?
      !!paste0(col_to_clean, "_clean") := dplyr::case_when(
        # Drug stuff -------------------------------------------------------------
        any_drugs & possess & !traffic_or_traffick & !distribution & !intent &
          !proceed & !paraphernalia & !dui_or_apc & !stamp & !weapon &
          !maintain_keep & !manufacture & !litter & !larceny & !jail_penal &
          !school & !park ~ "CDS Possession (Simple)",
        any_drugs & possess & (school | park | child) ~ "CDS Possession (Proximate to School, Park, or Minor)",
        # any_drugs & jail_penal ~ "CDS Possession (in Jail / Prison)",
        # Actually think I should just have a generic "contraband in jail" charge, that seems to be how it's used
        any_drugs & maintain_keep ~ "CDS Possesssion (Maintaining a Place)",
        any_drugs & larceny ~ "Larceny of a CDS",
        any_drugs & stamp ~ "CDS Possession (Tax Stamp)",
        any_drugs & paraphernalia ~ "CDS Paraphernalia Possession / Distribution",
        any_drugs & intent & possess & (traffic_or_traffick | distribution) ~ "CDS Possession With Intent (PWID)",
        any_drugs & (traffic_or_traffick | distribution) & !possess & !paraphernalia ~ "CDS Trafficking / Distribution",

        # DUI / APC related stuff ------------------------------------------------
        dui_or_apc ~ "DUI / APC",

        # Sex Work related stuff -------------------------------------------------
        sex_work & !aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Engaging in Sex Work (Simple)",
        sex_work & !aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Engaging in Sex Work (Within 1,000 Feet)",
        sex_work & aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Aiding / Abetting Sex Work (Simple)",
        sex_work & aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Aiding / Abetting Sex Work (Within 1,000 Feet)",
        sex_work & !aid_abet & child ~ "Engaging in Sex Work (Minor Involved)",
        sex_work & aid_abet & child ~ "Aiding / Abetting Sex Work (Minor Involved)",
        sex_work & (maintain_keep | operate) & !within_x_feet ~ "Maintaining / Operating Place for Sex Work (Simple)",
        sex_work & (maintain_keep | operate) & within_x_feet ~ "Maintaining / Operating Place for Sex Work (Within 1,000 Feet)",


        # Default to NA ----------------------------------------------------------
        TRUE ~ NA_character_
      ),
      # Cleaned charge CATEGORIES (i.e. "drug related", "property crime", "violent crime", etc.)
      # category = dplyr::case_when(...)
    )

  # true_clean_data is the original data + the final categories, no flags
  true_clean_data <- clean_data |>
    dplyr::select({{ col_to_clean }}, paste0(col_to_clean, "_clean"),
                  data_names)

  if(.keep_flags == TRUE) {
    # clean_data is just the version that still has the flags
    return(clean_data)
  } else {
    return(true_clean_data)
  }

}
