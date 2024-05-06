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
apply_ojo_regex <- function(data,
                            col_to_clean = "count_as_filed",
                            .keep_flags = FALSE
                            ) {

  # Validate data ==============================================================
  data_names <- names(data)
  clean_col_name <- paste0(col_to_clean, "_clean")

  # Check if col_to_clean exists in the data frame
  if (!col_to_clean %in% colnames(data)) {
    stop("Column not found in data frame.")
  }

  # Uncomment this for dev / debugging -----------------------------------------
  # Regex list (in progress)
  googlesheets4::gs4_auth(email = "abell@okpolicy.org")
  ojo_regex_flags <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing",
                                               sheet = "Regex Flag List")
  ojo_regex_cats <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1LyaUXb21OuBj5Cb0CewJ1lVMsVsExn6yOcfyDT5sqL0/edit?usp=sharing",
                                              sheet = "Clean Categories List",
                                              col_types = "lccccccccccccc") |>
    dplyr::filter(in_ojoregex == TRUE)

  # Save the regex data to the package data
  save(ojo_regex_flags, file = here::here("data", "ojo_regex_flags.rda"))
  save(ojo_regex_cats, file = here::here("data", "ojo_regex_cats.rda"))

  # Load the regex data
  regex <- ojo_regex_flags


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
  for (i in seq(nrow(regex))) {
    flagged_data <- apply_regex_pattern(flagged_data,
                                        regex$flag[i],
                                        regex$regex[i])
  }

  # ...then, apply the groups where relevant... ----------------------------------
  for (j in seq(nrow(group_data))) {
    group_flag <- group_data$group[j]
    flags <- unlist(stringr::str_split(group_data$list_flags[j], "\\|"))

    flagged_data <- flagged_data |>
      dplyr::mutate(!!group_flag := rowSums(dplyr::select(flagged_data, dplyr::all_of(flags)), na.rm = TRUE) > 0)
  }

  # ...now we have the flags in place, and we're ready to categorize!

  # Categorizing =================================================================

  clean_data <- flagged_data |>
    dplyr::mutate(
      # Cleaned charge descriptions (most specific, i.e. "simple possession", "kidnapping", etc.)
      # Later ones should overwrite previous ones, so maybe order by ascending priority?
      !!paste0(col_to_clean, "_clean") := dplyr::case_when(
        # Drug Crimes ==========================================================
        # Basic Drug Stuff -----------------------------------------------------
        any_drugs & possess & !traffic_or_traffick & !distribution & !intent &
          !proceed & !paraphernalia & !dui_or_apc & !stamp & !weapon &
          !maintain_keep & !manufacture & !litter & !larceny & !jail_penal &
          !school & !park ~ "CDS Possession (Simple)",
        any_drugs & possess & (school | park | child) ~ "CDS Possession (Proximate to School, Park, or Minor)",
        # any_drugs & jail_penal ~ "CDS Possession (in Jail / Prison)",
        # Actually think I should just have a generic "contraband in jail" charge, that seems to be how it's used
        any_drugs & maintain_keep ~ "CDS Possesssion (Maintaining a Place)",
        any_drugs & larceny ~ "Larceny of a CDS",
        any_drugs & paraphernalia ~ "CDS Paraphernalia Possession / Distribution",
        any_drugs & intent & possess & (traffic_or_traffick | distribution) ~ "CDS Possession With Intent (PWID)",
        any_drugs & (traffic_or_traffick | distribution) & !possess & !paraphernalia ~ "CDS Trafficking / Distribution",
        any_drugs & fraud ~ "Obtain CDS by Fraud",

        # Drug / Tax Stuff -----------------------------------------------------
        any_drugs & stamp ~ "CDS Possession (Tax Stamp)",

        # Property Crimes ======================================================
        # Larceny --------------------------------------------------------------
        larceny & grand & !petit & !any_drugs ~ "Larceny (Grand)",
        larceny & petit & !grand & !any_drugs ~ "Larceny (Petit)",
        (larceny & merchandise) | shoplift ~ "Larceny (Shoplifting)",
        (larceny | theft | steal) & automobile & !(false & report) ~ "Larceny (Auto)",
        larceny & !petit & !grand & !any_drugs & !(merchandise | shoplift) & !automobile ~ "Larceny (Other / Unspecified)", # Sometimes it lists none...
        larceny & petit & grand & !any_drugs ~ "Larceny (Other / Unspecified)", # ...and sometimes it lists all.
        theft & !identity & !credit_card & !(false & report) ~ "Larceny (Other / Unspecified)", # identity theft / credit card stuff is technically FRAUD, not LARCENY

        # RCSP -----------------------------------------------------------------
        ((property & (receive | conceal)) | kcsp | (rcsp_code & !credit_card)) & !rcspmv_code ~ "Receiving / Concealing Stolen Property",

        # Burglary -------------------------------------------------------------
        burgle & (first | one) ~ "Burglary (First Degree)",
        burgle & (second | two) ~ "Burglary (Second Degree)",
        burgle & (third | three | automobile) ~ "Burglary (Third Degree)",
        burgle & tools_implements ~ "Possession of Burglar's Tools",
        burgle & !first & !one & !second & !two & !third & !three & !tools_implements ~ "Burglary (Other / Unspecified)",
        enter & intent ~ "Entering with Intent To Commit a Crime",

        # Arson ----------------------------------------------------------------
        arson & (first | one | danger) ~ "Arson (First Degree)",
        arson & (second | two) ~ "Arson (Second Degree)",
        arson & (third | three) ~ "Arson (Third Degree)",
        arson & (fourth | four) ~ "Arson (Fourth Degree)",
        arson & !first & !one & !danger & !second & !two & !third & !three & !four ~ "Arson (Other / Unspecified)",

        # Fraud / Forgery ------------------------------------------------------
        personate ~ "Fraud (False Personation)",
        ((bogus & check) | bc_code) & !(pretense | deception) ~ "Fraud (Bogus Check)",
        (pretense | deception) & !(bogus & check) & !elder ~ "Fraud (False Pretense / Deception)", # Some forms of elder abuse include the term "deception"
        credit_card ~ "Fraud (Credit Card)", # May need more refining
        (forge | counterfeit) & !license & !bogus & !credit_card ~ "Fraud (Forgery / Counterfeiting)",
        (corporate & !embezzle) | (insurance & fraud) | (insurance & false) ~ "Fraud (Corporate / Insurance)",
        (pretense | deception) & (bogus & check) ~ "Fraud (Other / Unspecified)", # Sometimes both will be listed
        fraud & !personate & !pretense & !deception & !credit_card & !forge & !counterfeit & !corporate & !insurance & !any_drugs ~ "Fraud (Other / Unspecified)",

        # Embezzlement ---------------------------------------------------------
        embezzle ~ "Embezzlement",

        # Malicious Injury to Propert / minor property crimes ------------------
        malicious & injury & property ~ "Malicious Injury to Property",

        # Violent Crimes =======================================================
        # Assault / Battery ----------------------------------------------------
        (assault | battery | a_and_b | abuse | violence | abdom) & domestic & !weapon ~ "Domestic Assault / Battery (Simple)",
        (assault | battery | a_and_b | abuse | violence | abdom) & domestic & weapon ~ "Domestic Assault / Battery (Dangerous Weapon)",
        (assault | battery | a_and_b | abgen) & weapon & !domestic & !abdom ~ "Assault / Battery (Dangerous Weapon)",
        (assault | battery | a_and_b | abgen) & officer ~ "Assault / Battery (On Official)",
        (assault | battery | a_and_b | abgen) & !weapon & !domestic & !abdom & !sex & !officer ~ "Assault / Battery (Simple)",

        # Robbery --------------------------------------------------------------
        rob & (first | one | force | fear) & !(conjoint | two_or_more) ~ "Robbery (First Degree)",
        rob & (second | two) & !(conjoint | two_or_more) ~ "Robbery (Second Degree)",
        rob & weapon & !(conjoint | two_or_more) ~ "Robbery (With a Dangerous Weapon)",
        rob & (conjoint | two_or_more) ~ "Robbery (Conjoint)",
        rob & !first & !one & !second & !two & !conjoint & !two_or_more & !weapon & !extort ~ "Robbery (Other / Unspecified)",

        # Kidnapping -----------------------------------------------------------
        kidnap & !child & !extort & !traffic_or_traffick ~ "Kidnapping (Simple)",
        (kidnap | steal) & child & !traffic_or_traffick ~ "Kidnapping (Child Stealing)",
        kidnap & extort & !child & !traffic_or_traffick ~ "Kidnapping (Extortion)",
        human & traffic_or_traffick ~ "Kidnapping (Human Trafficking)",

        # Maiming --------------------------------------------------------------
        maim ~ "Maiming",

        # Other ================================================================
        # # Sex Work -------------------------------------------------------------
        # sex_work & !aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Engaging in Sex Work (Simple)",
        # sex_work & !aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Engaging in Sex Work (Within 1,000 Feet)",
        # sex_work & aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Aiding / Abetting Sex Work (Simple)",
        # sex_work & aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Aiding / Abetting Sex Work (Within 1,000 Feet)",
        # sex_work & !aid_abet & child ~ "Engaging in Sex Work (Minor Involved)",
        # sex_work & aid_abet & child ~ "Aiding / Abetting Sex Work (Minor Involved)",
        # sex_work & (maintain_keep | operate) & !within_x_feet ~ "Maintaining / Operating Place for Sex Work (Simple)",
        # sex_work & (maintain_keep | operate) & within_x_feet ~ "Maintaining / Operating Place for Sex Work (Within 1,000 Feet)",

        # Obstructing / Eluding ------------------------------------------------
        (resist | elude) & (officer | arrest) & !obstruct ~ "Resisting / Eluding Officer",
        (obstruct & (officer | justice)) |
          stringr::str_detect(count_as_filed, "(?i)obs, obst|^obstruct(ing|ion)$") ~ "Obstruction of Justice", # that last one is to catch "OBS, OBSTRUCTION" / "OBSTRUCTION" which are hard to capture with the flags
        # TODO: replace above with flags, I don't like having str_detect() in here

        # VPO ------------------------------------------------------------------
        vpo_code | (violate & protect) | (violate & vpo) | (stalk & vpo) ~ "Violation of Protective Order (VPO)",

        # Child abuse / neglect / violation of compulsory education act --------
        delinquent & !weapon | truant | (compulsory & education) | (school & (compel | refuse | neglect)) ~ "Violation of Compulsory Education Act",

        # Public Decency Crimes ------------------------------------------------
        public & (intoxication | drunk) ~ "Public Intoxication",
        (outrage | disturb) & decency ~ "Outraging Public Decency",
        (disturb | breach) & peace ~ "Disturbing the Peace",

        # Firearm Possession ---------------------------------------------------
        possess & weapon ~ "Illegal Possession of a Firearm",

        # Traffic / Motor Vehicles =============================================
        # Basic Traffic Stuff --------------------------------------------------
        (speeding | x_in_y | x_over) & !lane & !close_closely ~ "Speeding",
        seatbelt & !child ~ "Seatbelt Violation",
        seatbelt & child ~ "Child Seatbelt Violation",
        lane & !speeding ~ "Changing Lanes Unsafely", # Could potentially make more generic since it covers a few things, maybe "Unsafe Lane Use"?
        follow & close_closely ~ "Following Too Closely",
        stop & (sign | light) ~ "Fail to Stop at Sign",
        attention & !medical ~ "Inattentive Driving", # Originally had "drive" in here too, but some just say "INATTENTION" and stuff so this works better
        authorized & automobile & !license ~ "Unauthorized Use of Vehicle",

        # Driving without proper documentation ---------------------------------
        ((operate | drive) & (revocation | suspend)) | dus_code | dur_code ~ "Driving Under Suspension / Revocation",
        (operate | drive) & license & !tag & !suspend ~ "Driving Without Valid License",
        (operate | drive) & automobile & tag  ~ "Driving Without Proper Tag",
        fr5_code | ((failure | comply | no | compulsory) & insurance) ~ "Driving Without Valid Insurance",

        # DUI / APC / TOC / etc. -----------------------------------------------
        dui_or_apc & !weapon ~ "DUI / APC",
        toc | open & (container | bottle | beer) ~ "Transporting Open Container",

        # Stolen Vehicles ------------------------------------------------------
        (possess | receive) & automobile ~ "Possession of Stolen Vehicle",

        # Default to NA ========================================================
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
    return(clean_data) # clean_data is just the version that still has the flags
  } else {
    return(true_clean_data)
  }

}
