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

# Applying patterns ============================================================
# Function to apply the patterns
apply_regex_pattern <- function(data, flag, regex_pattern) {
  data |>
    mutate(
      !!flag := str_detect(count_as_filed,
                           stringr::regex(regex_pattern, ignore_case = TRUE))
    )
}

# Keeping the old version separate for debugging purposes
result <- test_data

# Apply function over every row of the dataset...
for(i in seq(nrow(regex))) {
  result <- apply_regex_pattern(result,
                                regex$flag[i],
                                regex$regex[i])
}

# ...then, apply the groups where relevant...
for (i in seq(nrow(group_data))) {
  group_flag <- group_data$group[i]
  flags <- unlist(str_split(group_data$list_flags[i], "\\|"))

  result <- result %>%
    mutate(!!group_flag := rowSums(select(result, all_of(flags)), na.rm = TRUE) > 0)
}

# ...now we have the flags in place, and we're ready to categorize!

# Categorizing =================================================================

categories <- result |>
  mutate(
    # Cleaned charge descriptions (most specific, i.e. "simple possession", "kidnapping", etc.)
    count_cleaned = case_when(
      # Drug stuff -------------------------------------------------------------
      any_drugs & !traffic_or_traffick & !distribution ~ "Drug Possession (Simple)",

      TRUE ~ NA_character_
    ),
    # Cleaned charge CATEGORIES (i.e. "drug related", "property crime", "violent crime", etc.)
    # category = case_when(...)
  )

