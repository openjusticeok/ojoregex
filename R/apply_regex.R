#' Apply OJO Regex
#'
#' @param data
#' @param col_to_clean
#'
#' @return
#' @export
#'
#' @examples
apply_ojo_regex <- function(data, col_to_clean, .keep_flags = FALSE) {

  library(ojodb)
  library(tidyverse)
  library(googlesheets4)

  # Validate data ==============================================================
  data <- data

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
    filter(!is.na(group)) |>
    group_by(group) |>
    summarize(
      list_flags = paste(flag, collapse = "|")
    )

  # Applying flags ===============================================================
  # Creating function to apply the patterns --------------------------------------
  apply_regex_pattern <- function(data, flag, regex_pattern) {
    data |>
      mutate(
        !!flag := str_detect(!!sym(col_to_clean),
                             stringr::regex(regex_pattern, ignore_case = TRUE))
      )
  }

  # Pre-cleaning steps -----------------------------------------------------------
  flagged_data <- data |>
    mutate(
      !!paste0(col_to_clean, "_clean") := ojoregex::regex_pre_clean(!!sym(col_to_clean))
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
    flags <- unlist(str_split(group_data$list_flags[j], "\\|"))

    flagged_data <- flagged_data %>%
      mutate(!!group_flag := rowSums(select(flagged_data, all_of(flags)), na.rm = TRUE) > 0)
  }

  # ...now we have the flags in place, and we're ready to categorize!

  # Categorizing =================================================================

  clean_data <- flagged_data |>
    mutate(
      # Cleaned charge descriptions (most specific, i.e. "simple possession", "kidnapping", etc.)
      # Later ones should overwrite previous ones, so maybe order by ascending priority?
      !!paste0(col_to_clean, "_clean") := case_when(
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
        sex_work & (maintain_keep | operate) & !within_x_feet ~ "Maintaing / Operating Place for Sex Work (Simple)",
        sex_work & (maintain_keep | operate) & within_x_feet ~ "Maintaing / Operating Place for Sex Work (Within 1,000 Feet)",


        # Default to NA ----------------------------------------------------------
        TRUE ~ NA_character_
      ),
      # Cleaned charge CATEGORIES (i.e. "drug related", "property crime", "violent crime", etc.)
      # category = case_when(...)
    )

  true_clean_data <- clean_data |>
    select({{ col_to_clean }}, paste0(col_to_clean, "_clean"))

    if(.keep_flags) {
      final <- data |>
        left_join(clean_data)
    } else{
      final <- data |>
        left_join(true_clean_data)
    }

  return(final)

}
