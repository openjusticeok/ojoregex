library(ojodb)
library(tidyverse)
library(googlesheets4)

# Test data ====================================================================
# test_data <- ojo_crim_cases(districts = "all",
#                             case_types = c("CF", "CM", "TR"),
#                             file_years = c(2000:2024)) |>
#   filter(!is.na(count_as_filed)) |>
#   count(count_as_filed, sort = T) |>
#   ojo_collect()
#
# readr::write_csv(test_data, here::here("data", "test_data.csv"))
test_data <- readr::read_csv(here::here("data", "test_data.csv"))

# Regex list (in progress):
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
      !!flag := str_detect(count_as_filed,
                           stringr::regex(regex_pattern, ignore_case = TRUE))
    )
}

# Keeping the old version separate for debugging purposes
flagged_data <- test_data

# Pre-cleaning steps -----------------------------------------------------------
flagged_data <- flagged_data |>
  mutate(
    # Remove "in concert ... [end of string]" (e.g. TAXS, FAIL TO DISPLAY TAX STAMP ON CDS IN CONCERT W/J POOLE)
    count_as_filed = str_remove_all(count_as_filed, stringr::regex("\\s*in(| )conc(ert|).*$", ignore_case = TRUE))
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

# this code should probably live in a function in R/

clean_data <- flagged_data |>
  mutate(
    # Cleaned charge descriptions (most specific, i.e. "simple possession", "kidnapping", etc.)
    # Later ones should overwrite previous ones, so maybe order by ascending priority?
    count_cleaned = case_when(
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

# Version w/out flag columns
true_clean_data <- clean_data |>
  select(count_as_filed, n, count_cleaned)

# Glimpse results
true_clean_data |>
  group_by(count_cleaned) |>
  slice_max(order_by = n, n = 100) |> view()

# Summary of total charges filed of each type
true_clean_data |>
  group_by(count_cleaned) |>
  summarize(total = sum(n)) |>
  arrange(desc(total))

