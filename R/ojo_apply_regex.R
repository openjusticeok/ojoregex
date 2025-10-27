#' Apply OJO Regex
#'
#' This function applies regular expressions patterns to clean and categorize charge descriptions in a given dataset.
#'
#' @param data A data frame containing the dataset to be processed.
#' @param col_to_clean The name of the column in the dataset containing the charge descriptions to be cleaned and categorized.
#' @param .keep_flags Logical value indicating whether to keep the concept flags generated during processing. Defaults to FALSE, which returns only the cleaned dataset without the flags.
#' @param .include_cats Logical value indiciating whether the categories / subcategories should be included in the returned data
#' @param .quiet Should the progress bar be shown?
#'
#' @return A cleaned and categorized dataset with charge descriptions in the specified column, along with any additional columns present in the original dataset.
#'
#' @importFrom rlang :=
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
ojo_apply_regex <- function(
  data,
  col_to_clean = "count_as_filed",
  .keep_flags = FALSE,
  .include_cats = TRUE,
  .quiet = FALSE
) {
  # Validate data ==============================================================
  data_names <- names(data)
  clean_col_name <- paste0(col_to_clean, "_clean")

  # Check if col_to_clean exists in the data frame
  if (!col_to_clean %in% colnames(data)) {
    stop("Column not found in data frame.")
  }

  # Load the regex data
  regex <- ojoregex::ojo_regex_flags

  # Creating a list of groups and their relevant flags also (like cds | meth | paraphernalia ... = any_drugs)
  # these should all start with any_ prefix
  group_data <- regex |>
    dplyr::filter(!is.na(group)) |>
    dplyr::group_by(group) |>
    dplyr::summarize(
      list_flags = paste(flag, collapse = "|")
    )

  # Applying flags ===============================================================
  # Pre-cleaning steps -----------------------------------------------------------
  distinct_charges <- data |>
    dplyr::distinct(!!dplyr::sym(col_to_clean)) |>
    dplyr::mutate(
      !!clean_col_name := ojoregex::regex_pre_clean(!!dplyr::sym(col_to_clean))
    )

  # Apply regex flags to the distinct charges ------------------------------------
  if (!.quiet) {
    cli::cli_progress_bar(
      "Applying regex flags...",
      total = nrow(regex)
    )
  }

  flags_list <- purrr::map(
    purrr::set_names(regex$regex, regex$flag),
    \(p) {
      if (!.quiet) {
        cli::cli_progress_update()
      }
      stringi::stri_detect(
        distinct_charges[[clean_col_name]],
        regex = paste0("(?i)", p)
      )
    }
  )

  if (!.quiet) {
    cli::cli_progress_done()
  }

  flagged_data <- dplyr::bind_cols(distinct_charges, flags_list)

  # ...then, apply the groups where relevant... ----------------------------------
  group_flags_list <- purrr::map(
    purrr::set_names(group_data$list_flags, group_data$group),
    \(flags_str) {
      flags <- unlist(stringr::str_split(flags_str, "\\|"))
      rowSums(dplyr::select(flagged_data, dplyr::all_of(flags)), na.rm = TRUE) >
        0
    }
  )

  flagged_data <- dplyr::bind_cols(flagged_data, group_flags_list)

  # ...now we have the flags in place, and we're ready to categorize!

  # Categorizing

  clean_data <- flagged_data |>
    dplyr::mutate(
      # Earlier ones will overwrite later ones, so the order is important!
      !!paste0(col_to_clean, "_clean") := dplyr::case_when(
        # ===================================================================================================================
        # Overriding charges ================================================================================================
        # These are at the top because I want them to override everything else. For example,
        # if a charge says "Accessory to Murder", I want it to be "Accessory" instead of "Murder"
        # Accessory to a felony ------------------------------------------------
        accessory ~ "Accessory to a Felony",

        # ====================================================================================================================
        # Drug Crimes ========================================================================================================
        # Basic Drug Stuff -----------------------------------------------------
        any_drugs &
          possess &
          !traffic_or_traffick &
          !distribution &
          !intent &
          !proceed &
          !paraphernalia &
          !dui_or_apc &
          !stamp &
          !weapon &
          !maintain_keep &
          !manufacture &
          !litter &
          !larceny &
          !jail_penal &
          !school &
          !park &
          !deliver ~
          "CDS Possession (Simple)",
        any_drugs & possess & (school | park | child) ~
          "CDS Possession (Proximate to School, Park, or Minor)",
        # any_drugs & jail_penal ~ "CDS Possession (in Jail / Prison)",
        # Actually think I should just have a generic "contraband in jail" charge, that seems to be how it's used
        any_drugs & maintain_keep ~ "CDS Possesssion (Maintaining a Place)",
        any_drugs & larceny ~ "Larceny of a CDS",
        any_drugs & paraphernalia ~
          "CDS Paraphernalia Possession / Distribution",
        any_drugs &
          intent &
          possess &
          (traffic_or_traffick | distribution) &
          !conspiracy ~
          "CDS Possession With Intent (PWID)",
        any_drugs &
          (traffic_or_traffick | distribution | deliver) &
          !possess &
          !paraphernalia &
          !conspiracy ~
          "CDS Trafficking / Distribution",
        any_drugs & fraud ~ "Obtain CDS by Fraud",
        # Sometimes it will just say "Marijuana", etc.
        any_drugs &
          !possess &
          !traffic_or_traffick &
          !distribution &
          !intent &
          !proceed &
          !paraphernalia &
          !dui_or_apc &
          !stamp &
          !weapon &
          !maintain_keep &
          !litter &
          !larceny &
          !jail_penal &
          !school &
          !park &
          !drive &
          !automobile &
          !under_the_influence &
          !throw &
          !(child & endanger) &
          !(open & container) ~
          "CDS (Other / Unspecified)",

        # Drug / Tax Stuff -----------------------------------------------------
        any_drugs & stamp ~ "CDS Possession (Tax Stamp)",

        # Acquiring proceeds ---------------------------------------------------
        # proceed & any_drugs & (acquire | possess | transport | conceal) ~ "Possession of Proceeds in Violation UCDSA",
        # proceed & !any_drugs & (acquire | possess | transport | conceal) ~ "Possession of Proceeds from Unlawful Activity",
        proceed & any_drugs & !bail ~
          "Possession of Proceeds in Violation UCDSA",
        proceed & !any_drugs & !bail ~
          "Possession of Proceeds from Unlawful Activity",

        # =====================================================================================================================
        # Property Crimes =====================================================================================================
        # Larceny --------------------------------------------------------------
        larceny & grand & !petit & !any_drugs ~ "Larceny (Grand)",
        larceny & petit & !grand & !any_drugs ~ "Larceny (Petit)",
        (larceny & merchandise) | shoplift ~ "Larceny (Shoplifting)",
        (larceny | theft | steal) & automobile & !(false & report) ~
          "Larceny (Auto)",
        larceny &
          !petit &
          !grand &
          !any_drugs &
          !(merchandise | shoplift) &
          !automobile ~
          "Larceny (Other / Unspecified)", # Sometimes it lists none...
        larceny & petit & grand & !any_drugs ~ "Larceny (Other / Unspecified)", # ...and sometimes it lists all.
        theft & !identity & !credit_card & !(false & report) ~
          "Larceny (Other / Unspecified)", # identity theft / credit card stuff is technically FRAUD, not LARCENY

        # RCSP -----------------------------------------------------------------
        ((property & (receive | conceal)) | kcsp | (rcsp_code & !credit_card)) &
          !rcspmv_code ~
          "Receiving / Concealing Stolen Property",

        # Burglary -------------------------------------------------------------
        burgle & (first | one) ~ "Burglary (First Degree)",
        burgle & (second | two) ~ "Burglary (Second Degree)",
        burgle & (third | three | automobile) ~ "Burglary (Third Degree)",
        burgle & tools_implements ~ "Possession of Burglar's Tools",
        burgle &
          !first &
          !one &
          !second &
          !two &
          !third &
          !three &
          !tools_implements ~
          "Burglary (Other / Unspecified)",
        (enter & intent) | (breaking & enter) ~
          "Entering with Intent To Commit a Crime",

        # Arson ----------------------------------------------------------------
        arson & (first | one | endanger) ~ "Arson (First Degree)", # This is actually a violent crime
        arson & (second | two) ~ "Arson (Second Degree)",
        arson & (third | three) ~ "Arson (Third Degree)",
        arson & (fourth | four) ~ "Arson (Fourth Degree)",
        arson &
          !first &
          !one &
          !danger &
          !second &
          !two &
          !third &
          !three &
          !four ~
          "Arson (Other / Unspecified)",

        # Fraud / Forgery ------------------------------------------------------
        personate | (identity & theft) ~ "Fraud (False Personation)",
        ((bogus & check) | bc_code) & !(pretense | deception) ~
          "Fraud (Bogus Check)",
        (pretense | deception) & !(bogus & check) & !elder ~
          "Fraud (False Pretense / Deception)", # Some forms of elder abuse include the term "deception"
        credit_card ~ "Fraud (Credit Card)", # May need more refining
        (forge | counterfeit) & !license & !bogus & !credit_card ~
          "Fraud (Forgery / Counterfeiting)",
        (corporate & !embezzle) | (insurance & fraud) | (insurance & false) ~
          "Fraud (Corporate / Insurance)",
        (pretense | deception) & (bogus & check) ~
          "Fraud (Other / Unspecified)", # Sometimes both will be listed
        fraud &
          !personate &
          !pretense &
          !deception &
          !credit_card &
          !forge &
          !counterfeit &
          !corporate &
          !insurance &
          !any_drugs ~
          "Fraud (Other / Unspecified)",

        # False declaration of ownership ---------------------------------------
        false & declaration ~ "False Declaration of Ownership",

        # Embezzlement ---------------------------------------------------------
        embezzle ~ "Embezzlement",

        # Malicious Injury to Property / minor property crimes -----------------
        malicious & injury ~ "Malicious Injury to Property", # removed "property" flag from this one to make it more inclusive

        # Trespassing ----------------------------------------------------------
        trespass & !rail & !timber ~ "Trespassing After Being Forbidden",
        trespass & rail & !timber ~
          "Trespassing / Destroying Railroad Equipment",
        trespass & !rail & timber ~ "Trespassing by Cutting Timber",

        # =====================================================================================================================
        # Crimes Against Children ======================================================================================================
        (child & (abuse | neglect | endanger | sex | injury | beating)) &
          !(lewd & indecent) &
          !school &
          !abdom &
          !a_and_b &
          !delinquent &
          !cdel &
          !(registration & provide) &
          !(offender & within_x_feet & license) &
          !murder &
          !fugitive &
          !(elude & officer) &
          !(traffic_or_traffick) &
          !steal ~
          "Child Abuse / Neglect / Sexual Abuse",
        (child | molest | proposal | act) &
          (lewd | indecent) &
          !(assault & battery) ~
          "Indecent or Lewd Acts With Child",
        (child & (omit | provide)) |
          (child & failure & !report) &
            !(seatbelt | restrain) &
            !(compel & school) &
            !(sex & offender) ~
          "Ommission to provide for child",
        (child & permit) &
          (beating |
            abuse |
            neglect |
            endanger |
            sex |
            manufacture) &
          !traffic_or_traffick ~
          "Permitting Child Abuse",
        (child & school) &
          (failure |
            report |
            abuse |
            neglect |
            endanger |
            sex |
            injury |
            beating) &
          !school &
          !within_x_feet &
          !zone_of_safety ~
          "School Superintendent or Administrator - Failure to Report Child Abuse and Neglect",
        (child & traffic_or_traffick) &
          !sex &
          !conspiracy ~
          "Trafficking in children",
        (child & (traffic_or_traffick | steal) & sex) & !transport ~
          "Child sex trafficking",
        (child & traffic_or_traffick & (sex | sex_work) & transport) ~
          "Offering or Transporting Child for Purpose of Child Sex Trafficking",
        (child &
          (second & degree) &
          (rape | sodomy | lewd | molest | sex | abuse)) ~
          "Second Offense - First Degree Rape, Sodomy, Lewd Molestation, Sexual Abuse of a Child",
        (child & pornography & aggravated) |
          (child & sex & material & aggravated) ~
          "Aggravated Possession of Child Pornography",
        (child & pornography & !aggravated) |
          (child & sex & material & !aggravated) ~
          "Purchase, Procurement, or Possession of Child Sexual Abuse Material",
        child & abandon & !animal ~ "Abandoning Child under Age Ten",
        child & desertion ~ "Desertion of Wife or Child Under 15",
        child & enticing & !(lewd | sex) ~
          "Maliciously, Forcibly or Fraudulently Taking or Enticing Away Children",
        harbor & (child | runaway) ~ "Harboring Runaway Child",
        (aid_abet | supervision | deprive | delinquent | cdel) &
          child &
          !school &
          !a_and_b &
          !rape &
          !(transport & sex_work) ~
          "Contributing/Causing Delinquency of Minors",

        # =====================================================================================================================
        # Violent Crimes ======================================================================================================
        # Murder / Intentional Homicide ----------------------------------------
        (shoot & kill & intent) |
          (weapon & automobile & !transport) |
          drive_by ~
          "Shooting With Intent to Kill",
        murder & (one | first) & !solicit ~ "Murder (First Degree)",
        murder & (two | second) & !solicit ~ "Murder (Second Degree)",
        murder & solicit ~ "Solicting Murder",
        murder & !(solicit | one | first | two | second) ~
          "Murder (Other / Unspecified)",

        # Manslaughter / Negligent Homicide ------------------------------------
        manslaughter & (one | first) ~ "Manslaughter (First Degree)",
        manslaughter & (two | second) ~ "Manslaughter (Second Degree)",
        manslaughter & !(one | first | two | second) & !fugitive ~
          "Manslaughter (Other / Unspecified)",
        homicide & negligent ~ "Negligent Vehicular Homicide",

        # Assault / Battery ----------------------------------------------------
        (assault | battery | a_and_b | abuse | violence | abdom) &
          domestic &
          !weapon ~
          "Domestic Assault / Battery (Simple)",
        (assault | battery | a_and_b | abuse | violence | abdom) &
          domestic &
          weapon ~
          "Domestic Assault / Battery (Dangerous Weapon)",
        (assault | battery | a_and_b | abgen) & weapon & !domestic & !abdom ~
          "Assault / Battery (Dangerous Weapon)",
        (assault | battery | a_and_b | abgen) & officer ~
          "Assault / Battery (On Official)",
        (assault | battery | a_and_b | abgen) &
          !weapon &
          !domestic &
          !abdom &
          !sex &
          !officer ~
          "Assault / Battery (Simple)",

        # Robbery --------------------------------------------------------------
        rob & (first | one | force | fear) & !(conjoint | two_or_more) ~
          "Robbery (First Degree)",
        rob & (second | two) & !(conjoint | two_or_more) ~
          "Robbery (Second Degree)",
        rob & weapon & !(conjoint | two_or_more) ~
          "Robbery (With a Dangerous Weapon)",
        rob & (conjoint | two_or_more) ~ "Robbery (Conjoint)",
        rob &
          !first &
          !one &
          !second &
          !two &
          !conjoint &
          !two_or_more &
          !weapon &
          !extort ~
          "Robbery (Other / Unspecified)",

        # Kidnapping -----------------------------------------------------------
        kidnap & !child & !extort & !traffic_or_traffick ~
          "Kidnapping (Simple)",
        (kidnap | steal) & child & !traffic_or_traffick ~
          "Kidnapping (Child Stealing)",
        kidnap & extort & !child & !traffic_or_traffick ~
          "Kidnapping (Extortion)",
        human & traffic_or_traffick & !child ~ "Kidnapping (Human Trafficking)",

        # Maiming --------------------------------------------------------------
        maim ~ "Maiming",

        # Rape -----------------------------------------------------------------
        (sex & battery) & !instrument ~ "Rape (First Degree)",
        rape & (first | one) & !instrument ~ "Rape (First Degree)",
        rape & (second | two) & !instrument ~ "Rape (Second Degree)",
        rape & instrument ~ "Rape by Instrumentation",
        sodomy ~ "Forcible Sodomy", # Not sure this is specific enough.
        rape & !first & !one & !second & !two & !instrument ~
          "Rape (Other / Unspecified)",

        # Pointing firearm -----------------------------------------------------
        point & weapon ~ "Pointing Weapon at Another",

        # =====================================================================================================================
        # Other ===============================================================================================================
        # # Sex Work -------------------------------------------------------------
        sex_work & !child & !maintain_keep & !operate ~
          "Engaging in Sex Work (Simple)",
        sex_work & child ~ "Engaging in Sex Work (Minor Involved)",
        sex_work & (maintain_keep | operate) ~
          "Maintaining / Operating Place for Sex Work",
        # sex_work & !aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Engaging in Sex Work (Simple)",
        # sex_work & !aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Engaging in Sex Work (Within 1,000 Feet)",
        # sex_work & aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Aiding / Abetting Sex Work (Simple)",
        # sex_work & aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Aiding / Abetting Sex Work (Within 1,000 Feet)",
        # sex_work & !aid_abet & child ~ "Engaging in Sex Work (Minor Involved)",
        # sex_work & aid_abet & child ~ "Aiding / Abetting Sex Work (Minor Involved)",
        # sex_work & (maintain_keep | operate) & !within_x_feet ~ "Maintaining / Operating Place for Sex Work (Simple)",
        # sex_work & (maintain_keep | operate) & within_x_feet ~ "Maintaining / Operating Place for Sex Work (Within 1,000 Feet)",

        # Obstructing / Eluding ------------------------------------------------
        flight_to_avoid | ((resist | elude) & (officer | arrest)) & !obstruct ~
          "Resisting / Eluding Officer",
        (obstruct & (officer | justice)) | obstruction_of_justice ~
          "Obstruction of Justice",

        # VPO / Stalking -------------------------------------------------------
        vpo_code |
          (violate & protect) |
          (violate & vpo) |
          (stalk & vpo) |
          (stalk & violate) ~
          "Violation of Protective Order (VPO)",
        stalk & !vpo ~ "Stalking",

        # Violation of compulsory education act --------------------------------
        delinquent &
          !weapon |
          truant |
          (compulsory & education) |
          (school & (compel | refuse | neglect)) ~
          "Violation of Compulsory Education Act",

        # Public Decency / Disturbing Peace Crimes -----------------------------
        public & (intoxication | drunk) ~ "Public Intoxication",
        (outrage | disturb) & decency ~ "Outraging Public Decency",
        (disturb | breach) & peace ~ "Disturbing the Peace",
        threat & violence ~ "Threaten or Plan Act of Violence",
        indecent & expose ~ "Indecent Exposure",

        # Firearm Possession ---------------------------------------------------
        (under_the_influence | intoxication) & weapon & !contraband ~
          "Carrying Firearm While Under the Influence",
        transport & weapon ~ "Improper Transportation of Firearms",
        (possess | carry | transfer) & weapon & !serial_number ~
          "Illegal Possession of a Firearm",
        child &
          weapon &
          delinquent ~ "Possession of a Firearm By Minor after Felony",
        ((possess | carry | use) & weapon & commit) | (weapon & serial_number) ~
          "Use of Firearm During Felony / Altering Serial Number",
        reckless & weapon ~ "Reckless Conduct With Firearm",
        discharge & weapon ~ "Reckless Discharge of Firearm",

        # Fugitive from justice ------------------------------------------------
        fugitive & !harbor ~ "Fugitive From Justice",
        fugitive & harbor ~ "Fugitive From Justice (Assisting / Harboring)",
        escape ~ "Escape from Arrest or Detention",

        # Bail jumping / bond forfeiture ---------------------------------------
        (bail | bond) & jump & !forfeit ~ "Bail Jumping",
        (bail | bond) & forfeit & !jump ~ "Bail Forfeiture",
        (bail | bond) & ((forfeit & jump) | (!forfeit & !jump)) ~
          "Bail Jumping", # Just gonna have these default to the more common one for now

        # Animal Cruelty / neglect ---------------------------------------------
        animal & cruel ~ "Cruelty to Animals",

        # Sex Offender related -------------------------------------------------
        # (registration | address) & sex & offender ~ "Failure to Comply With Sex Offender Registration Act",
        (sex & offender) & !within_x_feet & !zone_of_safety ~
          "Failure to Comply With Sex Offender Registration Act",
        (sex & offender) & within_x_feet & !zone_of_safety ~
          "Sex Offender Living Within 2000 Feet of School / Park / Child Care",
        zone_of_safety ~ "Sex Offender Violating Zone of Safety",
        unlawful &
          registration &
          child &
          sex &
          provide ~ "Unlawfully Providing Services to Children as Registered Sex Offender",

        # Violent crime registration related -----------------------------------
        (registration | address) & violence & (offender | comply | violate) ~
          "Failure to Comply With Violent Crime Offender Registration Act",

        # Emergency phone call -------------------------------------------------
        emergency & (phone | call) ~ "Interfering With Emergency Call",

        # Gang related offense -------------------------------------------------
        gang ~ "Gang Related Offense",

        # Abortion/Concealing death of child ------------------------------------
        ((abortion | miscarriage | (dead & child)) &
          !(abdom | domestic | abuse)) ~
          "Abortion/ Procuring Abortion/ Concealing Death of Child",
        perform & abortion & !(procure | conceal) ~
          "Persons Who May Perform Abortions",
        induce & abortion & !(procure | perform | conceal) ~
          "Self-Induced Abortions",
        conceal & (birth | dead) & child & !abortion ~
          "Concealing Birth or Death of Child",

        # Corpse
        authorized & remove & (corpse | body) ~
          "Unauthorized Removal of Dead Bodies",
        (desecrate | disrupt) &
          (corpse | body) &
          !conspiracy ~ "Desecration of Human Corpse",

        # Elders & Caretakers
        (abuse | neglect | exploit) &
          (by_caretaker | of_caretaker | caretaker) &
          !child &
          !conspiracy ~
          "Abuse, Neglect, or Financial Exploitation by Caretaker",
        (elder | disable) &
          exploit &
          !(child | by_caretaker | of_caretaker | caretaker) &
          !conspiracy ~
          "Exploitation of Elderly Persons or Disabled Adults",
        (verbal & abuse & (by_caretaker | of_caretaker | caretaker)) ~
          "Verbal Abuse by a Caretaker",

        # Tattoo
        (unlawful & (tattoo | piercing)) |
          (license & (tattoo | piercing)) |
          ((tattoo | piercing) & child) ~
          "Unlawful Body Tattooing, Body Piercing, and Scleral Tattooing",

        # Fluids & Waste
        ((jail_penal | officer) & (waste | fluid)) ~
          "Placing Body Wastes or Fluids on State Employee",

        # LEO Animals
        (mistreat | interfere) & officer & animal ~
          "Mistreating or Interfering with Police Dog or Horse",
        (kill | beat) & animal & officer ~ "Killing Police Dog or Horse",

        # =====================================================================================================================
        # Traffic / Motor Vehicles ============================================================================================
        # Basic Traffic Stuff --------------------------------------------------
        (speeding | x_in_y | x_over) & !lane & !close_closely ~ "Speeding",
        seatbelt & !child ~ "Seatbelt Violation",
        (seatbelt | restrain) & child ~ "Child Seatbelt Violation",
        lane & !speeding ~ "Changing Lanes Unsafely", # Could potentially make more generic since it covers a few things, maybe "Unsafe Lane Use"?
        follow & close_closely ~ "Following Too Closely",
        stop & (sign | light) ~ "Fail to Stop at Sign",
        attention & !medical ~ "Inattentive Driving", # Originally had "drive" in here too, but some just say "INATTENTION" and stuff so this works better
        authorized & automobile & !license ~ "Unauthorized Use of Vehicle",
        (reckless | careless) & drive ~ "Reckless Driving",
        failure & signal ~ "Fail to Signal",
        left & center ~ "Driving Left of Center",

        # Driving without proper documentation / tags / etc --------------------
        ((operate | drive | violate | possess | display) &
          (revocation | suspend)) |
          dus_code |
          dur_code |
          (suspend & license) ~
          "Driving Under Suspension / Revocation",
        (operate | drive | violate | possess | display | valid) &
          license &
          !tag &
          !suspend &
          !weapon ~
          "Driving Without Valid License",
        fr5_code |
          ((failure | comply | no | compulsory) & (insurance | secure)) ~
          "Driving Without Valid Insurance / Security",
        (operate | drive) & automobile & tag ~
          "Driving Without Proper Tag / Registration", # There are a couple of these...
        taxes_due ~ "Driving Without Proper Tag / Registration", # "taxes due to state"
        (registration | tag) &
          (expire | violate | improper | alter | illegal) &
          !sex &
          !violence ~
          "Driving Without Proper Tag / Registration",
        license & (improper | alter) ~
          "Driving Without Proper Tag / Registration", # "Altered / Improper license plates"

        # Defective equipment --------------------------------------------------
        defective & (automobile | brake | tire | light | equip | muffler) ~
          "Defective Vehicle",
        overweight ~ "Overweight Violation",

        # DUI / APC / TOC / etc. -----------------------------------------------
        (drive | automobile) &
          (influence | intoxication) |
          (dui_or_apc | under_the_influence & !weapon) ~
          "Driving Under the Influence / Actual Physical Control",
        (drive | automobile) & impair ~ "Driving While Impaired",
        toc | open & (container | bottle | beer) ~
          "Transporting Open Container",

        # Stolen Vehicles ------------------------------------------------------
        (possess | receive) & automobile ~ "Possession of Stolen Vehicle",

        # Leaving scene --------------------------------------------------------
        leave & scene ~ "Leaving the Scene of an Accident",

        # Failure to yield to emergency vehicle --------------------------------
        emergency & automobile ~ "Fail to Yield to Emergency Vehicle",

        # =====================================================================================================================
        # Defaults / special cases ============================================================================================
        # This is at the end so that anything with "conspiracy" that already hasn't been categorized
        # will get put down as "Conspiracy (Other / Unspecified)"
        conspiracy ~ "Conspiracy (Other / Unspecified)",
        hold_only ~ "Hold",

        # !!dplyr::sym(col_to_clean) == "DISMISSED" ~ "DISMISSED",
        dismiss ~ "ojoRegex Error: DISMISSED",
        count_x ~ "ojoRegex Error: COUNT X",
        TRUE ~ NA_character_
      ),
      # Cleaned charge CATEGORIES (i.e. "drug related", "property crime", "violent crime", etc.)
      # category = dplyr::case_when(...)
    )

  # Add original columns back on
  clean_data <- data |>
    dplyr::left_join(
      clean_data,
      by = {{ col_to_clean }},
      suffix = c("", "_flag") # If a column in data is the same as a flag name, add _flag suffix after
    )

  # Join on categories from the ojo_regex_cats data
  if (.include_cats) {
    ojo_regex_cats_tidy <- ojoregex::ojo_regex_cats |>
      dplyr::select(
        "clean_charge_description",
        "category",
        "subcategory",
        "title",
        "statutes",
        "chapter",
        "cf_cm",
        "sq780_status",
        "violent_crimes_list",
        "control_rank"
      )

    clean_data <- clean_data |>
      dplyr::left_join(
        ojo_regex_cats_tidy,
        by = dplyr::join_by({{ clean_col_name }} == "clean_charge_description")
      )

    # true_clean_data is the original data + the final categories, WITHOUT FLAGS
    true_clean_data <- clean_data |>
      dplyr::select(
        {{ col_to_clean }},
        paste0(col_to_clean, "_clean"),
        data_names,
        "category",
        "subcategory",
        "title",
        "statutes",
        "chapter",
        "cf_cm",
        "sq780_status",
        "violent_crimes_list",
        "control_rank"
      ) # Might not be needed long term?
  } else {
    # if .include_cats == FALSE, then skip adding the categories dataset
    true_clean_data <- clean_data |>
      dplyr::select(
        {{ col_to_clean }},
        paste0(col_to_clean, "_clean"),
        data_names,
      )
  }

  if (.keep_flags == TRUE) {
    return(clean_data) # clean_data is just the version that still has the flags
  } else {
    return(true_clean_data)
  }
}
