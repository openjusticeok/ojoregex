# In this script, we build the dataset that will be used to categorize
# the charges based on the flags that are generated in the ojo_apply_regex.R
# script.

ojo_regex_rules <- tibble::tribble(
  ~category,
  ~condition,
  # ===================================================================================================================
  # Overriding charges ===============================================================================================
  # These are at the top because I want them to override everything else. For example,
  # if a charge says "Accessory to Murder", I want it to be "Accessory" instead of "Murder"
  # Accessory to a felony ------------------------------------------------
  "Accessory to a Felony",
  rlang::quo(accessory),

  # ===================================================================================================================
  # Drug Crimes =======================================================================================================
  # Basic Drug Stuff -----------------------------------------------------
  "CDS Possession (Simple)",
  rlang::quo(
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
      !deliver
  ),
  "CDS Possession (Proximate to School, Park, or Minor)",
  rlang::quo(any_drugs & possess & (school | park | child)),
  # any_drugs & jail_penal ~ "CDS Possession (in Jail / Prison)",
  # Actually think I should just have a generic "contraband in jail" charge, that seems to be how it's used
  "CDS Possesssion (Maintaining a Place)",
  rlang::quo(any_drugs & maintain_keep),
  "Larceny of a CDS",
  rlang::quo(any_drugs & larceny),
  "CDS Paraphernalia Possession / Distribution",
  rlang::quo(any_drugs & paraphernalia),
  "CDS Possession With Intent (PWID)",
  rlang::quo(
    any_drugs & intent & possess & (traffic_or_traffick | distribution)
  ),
  "CDS Trafficking / Distribution",
  rlang::quo(
    any_drugs &
      (traffic_or_traffick | distribution | deliver) &
      !possess &
      !paraphernalia
  ),
  "Obtain CDS by Fraud",
  rlang::quo(any_drugs & fraud),
  # Sometimes it will just say "Marijuana", etc.
  "CDS (Other / Unspecified)",
  rlang::quo(
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
      !throw
  ),

  # Drug / Tax Stuff -----------------------------------------------------
  "CDS Possession (Tax Stamp)",
  rlang::quo(any_drugs & stamp),

  # Acquiring proceeds ---------------------------------------------------
  # proceed & any_drugs & (acquire | possess | transport | conceal) ~ "Possession of Proceeds in Violation UCDSA",
  # proceed & !any_drugs & (acquire | possess | transport | conceal) ~ "Possession of Proceeds from Unlawful Activity",
  "Possession of Proceeds in Violation UCDSA",
  rlang::quo(proceed & any_drugs & !bail),
  "Possession of Proceeds from Unlawful Activity",
  rlang::quo(proceed & !any_drugs & !bail),

  # =====================================================================================================================
  # Property Crimes =====================================================================================================
  # Larceny --------------------------------------------------------------
  "Larceny (Grand)",
  rlang::quo(larceny & grand & !petit & !any_drugs),
  "Larceny (Petit)",
  rlang::quo(larceny & petit & !grand & !any_drugs),
  "Larceny (Shoplifting)",
  rlang::quo((larceny & merchandise) | shoplift),
  "Larceny (Auto)",
  rlang::quo((larceny | theft | steal) & automobile & !(false & report)),
  "Larceny (Other / Unspecified)",
  rlang::quo(
    larceny &
      !petit &
      !grand &
      !any_drugs &
      !(merchandise | shoplift) &
      !automobile
  ), # Sometimes it lists none...
  "Larceny (Other / Unspecified)",
  rlang::quo(larceny & petit & grand & !any_drugs), # ...and sometimes it lists all.
  "Larceny (Other / Unspecified)",
  rlang::quo(theft & !identity & !credit_card & !(false & report)), # identity theft / credit card stuff is technically FRAUD, not LARCENY

  # RCSP -----------------------------------------------------------------
  "Receiving / Concealing Stolen Property",
  rlang::quo(
    ((property & (receive | conceal)) | kcsp | (rcsp_code & !credit_card)) &
      !rcspmv_code
  ),

  # Burglary -------------------------------------------------------------
  "Burglary (First Degree)",
  rlang::quo(burgle & (first | one)),
  "Burglary (Second Degree)",
  rlang::quo(burgle & (second | two)),
  "Burglary (Third Degree)",
  rlang::quo(burgle & (third | three | automobile)),
  "Possession of Burglar's Tools",
  rlang::quo(burgle & tools_implements),
  "Burglary (Other / Unspecified)",
  rlang::quo(
    burgle &
      !first &
      !one &
      !second &
      !two &
      !third &
      !three &
      !tools_implements
  ),
  "Entering with Intent To Commit a Crime",
  rlang::quo((enter & intent) | (breaking & enter)),

  # Arson ----------------------------------------------------------------
  "Arson (First Degree)",
  rlang::quo(arson & (first | one | danger)), # This is actually a violent crime
  "Arson (Second Degree)",
  rlang::quo(arson & (second | two)),
  "Arson (Third Degree)",
  rlang::quo(arson & (third | three)),
  "Arson (Fourth Degree)",
  rlang::quo(arson & (fourth | four)),
  "Arson (Other / Unspecified)",
  rlang::quo(
    arson &
      !first &
      !one &
      !danger &
      !second &
      !two &
      !third &
      !three &
      !four
  ),

  # Fraud / Forgery ------------------------------------------------------
  "Fraud (False Personation)",
  rlang::quo(personate | (identity & theft)),
  "Fraud (Bogus Check)",
  rlang::quo(((bogus & check) | bc_code) & !(pretense | deception)),
  "Fraud (False Pretense / Deception)",
  rlang::quo((pretense | deception) & !(bogus & check) & !elder), # Some forms of elder abuse include the term "deception"
  "Fraud (Credit Card)",
  rlang::quo(credit_card), # May need more refining
  "Fraud (Forgery / Counterfeiting)",
  rlang::quo((forge | counterfeit) & !license & !bogus & !credit_card),
  "Fraud (Corporate / Insurance)",
  rlang::quo(
    (corporate & !embezzle) | (insurance & fraud) | (insurance & false)
  ),
  "Fraud (Other / Unspecified)",
  rlang::quo((pretense | deception) & (bogus & check)), # Sometimes both will be listed
  "Fraud (Other / Unspecified)",
  rlang::quo(
    fraud &
      !personate &
      !pretense &
      !deception &
      !credit_card &
      !forge &
      !counterfeit &
      !corporate &
      !insurance &
      !any_drugs
  ),

  # False declaration of ownership ---------------------------------------
  "False Declaration of Ownership",
  rlang::quo(false & declaration),

  # Embezzlement ---------------------------------------------------------
  "Embezzlement",
  rlang::quo(embezzle),

  # Malicious Injury to Property / minor property crimes -----------------
  "Malicious Injury to Property",
  rlang::quo(malicious & injury), # removed "property" flag from this one to make it more inclusive

  # Trespassing ----------------------------------------------------------
  "Trespassing After Being Forbidden",
  rlang::quo(trespass & !rail & !timber),
  "Trespassing / Destroying Railroad Equipment",
  rlang::quo(trespass & rail & !timber),
  "Trespassing by Cutting Timber",
  rlang::quo(trespass & !rail & timber),

  # =====================================================================================================================
  # Violent Crimes ======================================================================================================
  # Murder / Intentional Homicide ----------------------------------------
  "Shooting With Intent to Kill",
  rlang::quo(
    (shoot & kill & intent) |
      (weapon & automobile & !transport) |
      drive_by
  ),
  "Murder (First Degree)",
  rlang::quo(murder & (one | first) & !solicit),
  "Murder (Second Degree)",
  rlang::quo(murder & (two | second) & !solicit),
  "Solicting Murder",
  rlang::quo(murder & solicit),
  "Murder (Other / Unspecified)",
  rlang::quo(murder & !(solicit | one | first | two | second)),

  # Manslaughter / Negligent Homicide ------------------------------------
  "Manslaughter (First Degree)",
  rlang::quo(manslaughter & (one | first)),
  "Manslaughter (Second Degree)",
  rlang::quo(manslaughter & (two | second)),
  "Manslaughter (Other / Unspecified)",
  rlang::quo(manslaughter & !(one | first | two | second)),
  "Negligent Vehicular Homicide",
  rlang::quo(homicide & negligent),

  # Assault / Battery ----------------------------------------------------
  "Domestic Assault / Battery (Simple)",
  rlang::quo(
    (assault | battery | a_and_b | abuse | violence | abdom) &
      domestic &
      !weapon
  ),
  "Domestic Assault / Battery (Dangerous Weapon)",
  rlang::quo(
    (assault | battery | a_and_b | abuse | violence | abdom) &
      domestic &
      weapon
  ),
  "Assault / Battery (Dangerous Weapon)",
  rlang::quo(
    (assault | battery | a_and_b | abgen) & weapon & !domestic & !abdom
  ),
  "Assault / Battery (On Official)",
  rlang::quo((assault | battery | a_and_b | abgen) & officer),
  "Assault / Battery (Simple)",
  rlang::quo(
    (assault | battery | a_and_b | abgen) &
      !weapon &
      !domestic &
      !abdom &
      !sex &
      !officer
  ),

  # Robbery --------------------------------------------------------------
  "Robbery (First Degree)",
  rlang::quo(rob & (first | one | force | fear) & !(conjoint | two_or_more)),
  "Robbery (Second Degree)",
  rlang::quo(rob & (second | two) & !(conjoint | two_or_more)),
  "Robbery (With a Dangerous Weapon)",
  rlang::quo(rob & weapon & !(conjoint | two_or_more)),
  "Robbery (Conjoint)",
  rlang::quo(rob & (conjoint | two_or_more)),
  "Robbery (Other / Unspecified)",
  rlang::quo(
    rob &
      !first &
      !one &
      !second &
      !two &
      !conjoint &
      !two_or_more &
      !weapon &
      !extort
  ),

  # Kidnapping -----------------------------------------------------------
  "Kidnapping (Simple)",
  rlang::quo(kidnap & !child & !extort & !traffic_or_traffick),
  "Kidnapping (Child Stealing)",
  rlang::quo((kidnap | steal) & child & !traffic_or_traffick),
  "Kidnapping (Extortion)",
  rlang::quo(kidnap & extort & !child & !traffic_or_traffick),
  "Kidnapping (Human Trafficking)",
  rlang::quo(human & traffic_or_traffick),

  # Maiming --------------------------------------------------------------
  "Maiming",
  rlang::quo(maim),

  # Child Abuse ----------------------------------------------------------
  "Child Abuse / Neglect / Sexual Abuse",
  rlang::quo((child & (abuse | neglect | danger)) & !school),
  # Does this include OMIT TO PROVIDE ?
  # This is where we'd distinguish if we want
  "Indecent or Lewd Acts With Child",
  rlang::quo((child | molest | proposal | act) & (lewd | indecent)),

  # Rape -----------------------------------------------------------------
  "Rape (First Degree)",
  rlang::quo((sex & battery) & !instrument),
  "Rape (First Degree)",
  rlang::quo(rape & (first | one) & !instrument),
  "Rape (Second Degree)",
  rlang::quo(rape & (second | two) & !instrument),
  "Rape by Instrumentation",
  rlang::quo(rape & instrument),
  "Forcible Sodomy",
  rlang::quo(sodomy), # Not sure this is specific enough.
  "Rape (Other / Unspecified)",
  rlang::quo(rape & !first & !one & !second & !two & !instrument),

  # Pointing firearm -----------------------------------------------------
  "Pointing Weapon at Another",
  rlang::quo(point & weapon),

  # =====================================================================================================================
  # Other ===============================================================================================================
  # # Sex Work -------------------------------------------------------------
  "Engaging in Sex Work (Simple)",
  rlang::quo(sex_work & !child & !maintain_keep & !operate),
  "Engaging in Sex Work (Minor Involved)",
  rlang::quo(sex_work & child),
  "Maintaining / Operating Place for Sex Work",
  rlang::quo(sex_work & (maintain_keep | operate)),
  # sex_work & !aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Engaging in Sex Work (Simple)",
  # sex_work & !aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Engaging in Sex Work (Within 1,000 Feet)",
  # sex_work & aid_abet & !child & !maintain_keep & !operate & !within_x_feet ~ "Aiding / Abetting Sex Work (Simple)",
  # sex_work & aid_abet & !child & !maintain_keep & !operate & within_x_feet ~ "Aiding / Abetting Sex Work (Within 1,000 Feet)",
  # sex_work & !aid_abet & child ~ "Engaging in Sex Work (Minor Involved)",
  # sex_work & aid_abet & child ~ "Aiding / Abetting Sex Work (Minor Involved)",
  # sex_work & (maintain_keep | operate) & !within_x_feet ~ "Maintaining / Operating Place for Sex Work (Simple)",
  # sex_work & (maintain_keep | operate) & within_x_feet ~ "Maintaining / Operating Place for Sex Work (Within 1,000 Feet)",

  # Obstructing / Eluding ------------------------------------------------
  "Resisting / Eluding Officer",
  rlang::quo(
    flight_to_avoid | ((resist | elude) & (officer | arrest)) & !obstruct
  ),
  "Obstruction of Justice",
  rlang::quo((obstruct & (officer | justice)) | obstruction_of_justice),

  # VPO / Stalking -------------------------------------------------------
  "Violation of Protective Order (VPO)",
  rlang::quo(
    vpo_code |
      (violate & protect) |
      (violate & vpo) |
      (stalk & vpo) |
      (stalk & violate)
  ),
  "Stalking",
  rlang::quo(stalk & !vpo),

  # Violation of compulsory education act --------------------------------
  "Violation of Compulsory Education Act",
  rlang::quo(
    delinquent &
      !weapon |
      truant |
      (compulsory & education) |
      (school & (compel | refuse | neglect))
  ),
  # child & neglect

  # Public Decency / Disturbing Peace Crimes -----------------------------
  "Public Intoxication",
  rlang::quo(public & (intoxication | drunk)),
  "Outraging Public Decency",
  rlang::quo((outrage | disturb) & decency),
  "Disturbing the Peace",
  rlang::quo((disturb | breach) & peace),
  "Threaten or Plan Act of Violence",
  rlang::quo(threat & violence),
  "Indecent Exposure",
  rlang::quo(indecent & expose),

  # Firearm Possession --------------------------------------------------
  "Carrying Firearm While Under the Influence",
  rlang::quo((under_the_influence | intoxication) & weapon & !contraband),
  "Improper Transportation of Firearms",
  rlang::quo(transport & weapon),
  "Illegal Possession of a Firearm",
  rlang::quo((possess | carry | transfer) & weapon & !serial_number),
  "Use of Firearm During Felony / Altering Serial Number",
  rlang::quo(
    ((possess | carry | use) & weapon & commit) | (weapon & serial_number)
  ),
  "Reckless Conduct With Firearm",
  rlang::quo(reckless & weapon),
  "Reckless Discharge of Firearm",
  rlang::quo(discharge & weapon),

  # Fugitive from justice ------------------------------------------------
  "Fugitive From Justice",
  rlang::quo(fugitive & !harbor),
  "Fugitive From Justice (Assisting / Harboring)",
  rlang::quo(fugitive & harbor),
  "Escape from Arrest or Detention",
  rlang::quo(escape),

  # Bail jumping / bond forfeiture ---------------------------------------
  "Bail Jumping",
  rlang::quo((bail | bond) & jump & !forfeit),
  "Bail Forfeiture",
  rlang::quo((bail | bond) & forfeit & !jump),
  "Bail Jumping",
  rlang::quo((bail | bond) & ((forfeit & jump) | (!forfeit & !jump))), # Just gonna have these default to the more common one for now

  # Animal Cruelty / neglect ---------------------------------------------
  "Cruelty to Animals",
  rlang::quo(animal & cruel),

  # Sex Offender related -------------------------------------------------
  # (registration | address) & sex & offender ~ "Failure to Comply With Sex Offender Registration Act",
  "Failure to Comply With Sex Offender Registration Act",
  rlang::quo((sex & offender) & !within_x_feet & !zone_of_safety),
  "Sex Offender Living Within 2000 Feet of School / Park / Child Care",
  rlang::quo((sex & offender) & within_x_feet & !zone_of_safety),
  "Sex Offender Violating Zone of Safety",
  rlang::quo(zone_of_safety),

  # Violent crime registration related -----------------------------------
  "Failure to Comply With Violent Crime Offender Registration Act",
  rlang::quo(
    (registration | address) & violence & (offender | comply | violate)
  ),

  # Emergency phone call -------------------------------------------------
  "Interfering With Emergency Call",
  rlang::quo(emergency & (phone | call)),

  # Gang related offense -------------------------------------------------
  "Gang Related Offense",
  rlang::quo(gang),

  # =====================================================================================================================
  # Traffic / Motor Vehicles ============================================================================================
  # Basic Traffic Stuff --------------------------------------------------
  "Speeding",
  rlang::quo((speeding | x_in_y | x_over) & !lane & !close_closely),
  "Seatbelt Violation",
  rlang::quo(seatbelt & !child),
  "Child Seatbelt Violation",
  rlang::quo((seatbelt | restrain) & child),
  "Changing Lanes Unsafely",
  rlang::quo(lane & !speeding), # Could potentially make more generic since it covers a few things, maybe "Unsafe Lane Use"?
  "Following Too Closely",
  rlang::quo(follow & close_closely),
  "Fail to Stop at Sign",
  rlang::quo(stop & (sign | light)),
  "Inattentive Driving",
  rlang::quo(attention & !medical), # Originally had "drive" in here too, but some just say "INATTENTION" and stuff so this works better
  "Unauthorized Use of Vehicle",
  rlang::quo(authorized & automobile & !license),
  "Reckless Driving",
  rlang::quo((reckless | careless) & drive),
  "Fail to Signal",
  rlang::quo(failure & signal),
  "Driving Left of Center",
  rlang::quo(left & center),

  # Driving without proper documentation / tags / etc --------------------
  "Driving Under Suspension / Revocation",
  rlang::quo(
    ((operate | drive | violate | possess | display) &
      (revocation | suspend)) |
      dus_code |
      dur_code |
      (suspend & license)
  ),
  "Driving Without Valid License",
  rlang::quo(
    (operate | drive | violate | possess | display | valid) &
      license &
      !tag &
      !suspend &
      !weapon
  ),
  "Driving Without Valid Insurance / Security",
  rlang::quo(
    fr5_code |
      ((failure | comply | no | compulsory) & (insurance | secure))
  ),
  "Driving Without Proper Tag / Registration",
  rlang::quo((operate | drive) & automobile & tag), # There are a couple of these...
  "Driving Without Proper Tag / Registration",
  rlang::quo(taxes_due), # "taxes due to state"
  "Driving Without Proper Tag / Registration",
  rlang::quo(
    (registration | tag) &
      (expire | violate | improper | alter | illegal) &
      !sex &
      !violence
  ),
  "Driving Without Proper Tag / Registration",
  rlang::quo(license & (improper | alter)), # "Altered / Improper license plates"

  # Defective equipment --------------------------------------------------
  "Defective Vehicle",
  rlang::quo(defective & (automobile | brake | tire | light | equip | muffler)),
  "Overweight Violation",
  rlang::quo(overweight),

  # DUI / APC / TOC / etc. -----------------------------------------------
  "Driving Under the Influence / Actual Physical Control",
  rlang::quo(
    (drive | automobile) &
      (influence | intoxication) |
      (dui_or_apc | under_the_influence & !weapon)
  ),
  "Driving While Impaired",
  rlang::quo((drive | automobile) & impair),
  "Transporting Open Container",
  rlang::quo(toc | open & (container | bottle | beer)),

  # Stolen Vehicles ------------------------------------------------------
  "Possession of Stolen Vehicle",
  rlang::quo((possess | receive) & automobile),

  # Leaving scene --------------------------------------------------------
  "Leaving the Scene of an Accident",
  rlang::quo(leave & scene),

  # Failure to yield to emergency vehicle --------------------------------
  "Fail to Yield to Emergency Vehicle",
  rlang::quo(emergency & automobile),
  
  # =====================================================================================================================
  # Defaults / special cases ============================================================================================
  # This is at the end so that anything with "conspiracy" that already hasn't been categorized
  # will get put down as "Conspiracy (Other / Unspecified)"

  "Conspiracy (Other / Unspecified)", 
  rlang::quo(conspiracy),
  
  "Hold",
  rlang::quo(hold_only),

  # !!dplyr::sym(col_to_clean) == "DISMISSED" ~ "DISMISSED",
  "ojoRegex Error: DISMISSED",
  rlang::quo(dismiss),
  "ojoRegex Error: COUNT X",
  rlang::quo(count_x),
  
  NA_character_,
  rlang::quo(TRUE)
)

usethis::use_data(ojo_regex_rules, overwrite = TRUE)
