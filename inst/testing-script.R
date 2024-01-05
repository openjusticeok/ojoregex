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

clean_data <- flagged_data |>
  mutate(
    # Cleaned charge descriptions (most specific, i.e. "simple possession", "kidnapping", etc.)
    # Later ones should overwrite previous ones, so maybe order by ascending priority?
    count_cleaned = case_when(
      # Drug stuff -------------------------------------------------------------
      any_drugs & !traffic_or_traffick & !distribution & !intent & !proceed &
        !paraphernalia & !dui_or_apc & !stamp & !weapon ~ "Drug Possession (Simple)",
        # maybe needs one for "KEEPING / MAINTAING PLACE" ?
        # needs execption for "CARRYING A FIREARM WHILE UNDER THE INFLUENCE"
        # need to incorporate "MANUFACTUR" flag into regex and add exception
        # need to incorporate "MAINTAINING PLACE" flag into regex and add exception
        # exception for "Littering Flaming or Glowing Substance from a Motor Vehicle"
        # exception for "Jail / penal institution"
        # exception for "larceny of CDS"
      any_drugs & tax & stamp ~ "Drug Possession (Tax Stamp)",
      any_drugs & paraphernalia ~ "Drug Paraphernalia Possession / Distribution",
      any_drugs & intent & possess & (traffic_or_traffick | distribution) ~ "Drug Possession With Intent (PWID)",
      any_drugs & (traffic_or_traffick | distribution) & !possess & !paraphernalia ~ "Drug Trafficking",

      TRUE ~ NA_character_
    ),
    # Cleaned charge CATEGORIES (i.e. "drug related", "property crime", "violent crime", etc.)
    # category = case_when(...)
  )

true_clean_data <- clean_data |>
  select(count_as_filed, n, count_cleaned)

true_clean_data |>
  group_by(count_cleaned) |>
  slice_max(order_by = n, n = 10) |>
  print(n = 40)

