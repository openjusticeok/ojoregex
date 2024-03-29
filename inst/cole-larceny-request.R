devtools::load_all()
library(ojodb)
library(tidyverse)
library(here)

if (!fs::file_exists(here("data", "crim-data.rds"))) {
  ds <- ojo_crim_cases(
    districts = "all",
    case_types = c("CF", "CM"),
    file_years = 2015:2023
  ) |>
    ojo_collect()

  fs::dir_create(here("data"))

  write_rds(ds, here("data/crim-data.rds"))
}

ds <- read_rds(here("data", "crim-data.rds"))

# Using new regex --------------------------------------------------------------

final <- ds |>
  ojoregex::apply_ojo_regex(col_to_clean = "count_as_filed", .keep_flags = TRUE)

# Add theft --------------------------------------------------------------

# final <- final |>
#   mutate(
#     count_as_filed_clean = case_when(
#       !is.na(count_as_filed_clean) ~ count_as_filed_clean,
#       theft & !identity ~ "Larceny (Theft)",
#       theft & identity ~ "Identity Theft",
#       TRUE ~ count_as_filed_clean
#     )
#   )

final |>
  # filter(theft & !identity) |>
  count(count_as_filed, count_as_filed_clean, sort = T)

#------------------------------------------------------------------------------


# Remaining unclassifieds
remaining_nas <- final |>
  filter(is.na(count_as_filed_clean)) |>
  select(-c(id, district, case_number, case_type, date_filed, date_closed, counts, open_counts, disposition,
            count_as_filed_clean)) |>
  group_by(count_as_filed) |>
  mutate(n = n()) |>
  distinct(count_as_filed, .keep_all = TRUE) |>
  select(count_as_filed, n, everything()) |>
  arrange(desc(n))

# Unique charge descriptions for positive matches
final |>
  filter(str_detect(count_as_filed_clean, "(?i)larc")) |>
  count(count_as_filed, count_as_filed_clean, sort = T)

# Unique charge descriptions for negative matches
final |>
  filter(!str_detect(count_as_filed_clean, "(?i)larc")) |>
  count(count_as_filed, count_as_filed_clean, sort = T)

final |>
  filter(count_as_filed_clean == "Larceny (Other / Unspecified)") |>
  count(count_as_filed, sort = T)

# Ashley regex ----------------------------------------------------------------
ashley_data <- ds |>
  mutate(
    # 780 property regex -------------------------------------------------------
    larceny = str_detect(count_as_filed, "(?i)larceny|larc|lar ceny|larency|lareny|theft|LMFR|larceny of merchandise|meat|corporeal property|\\bstol(e|en)\\b|\\blarc\\b") & !str_detect(count_as_filed, "(?i)controlled|cds|amended|\\bfalse|\\btrick|\\bco(n|)c(ea|ae)l|\\bbring|\\bknow|\\breceiv|\\btrans|\\bposs"),
    receive_stolen = str_detect(count_as_filed, "(?i)RCSP|stolen property|stoeln|concealing stolen|receive|obtain(ing|) (cash|money|property)|\\bposs") & !str_detect(count_as_filed, "(?i)radio|\\bexploitation|\\bcomputer|\\blarc|\\bchild|\\bdrug|\\bmarji|\\bcds\\b|\\bpara|\\bcontrol|\\bpenal|\\bminor|\\bintent|\\bbogus|\\bfalse|\\btrick|\\bimperson|\\bconfid"),
    embezzle = str_detect(count_as_filed, "(?i)embezzlement|embezlement|emblezzlement|embzzlement"),
    misc = str_detect(count_as_filed, "(?i)domesticated game|\\boil\\b|\\bdrilling\\b|\\bgas\\b|\\bhome rep") & !str_detect(count_as_filed, "(?i)alcohol|hash"),
    pawn = str_detect(count_as_filed, "(?i)repay pawn|pawnbroker|pawn shop|in pawn"),
    hospitality = str_detect(count_as_filed, "(?i)(hotel|inn|restaurant|boarding house|rooming house|motel|auto camp|trailer camp|apartment|rental unit|rental house)") & !str_detect(count_as_filed, "(?i)driving|virus"),
    false_pretense = str_detect(count_as_filed, "(?i)false pretense|\\bfalse decla|con game|confidence game|false representation|trick or deception|\\bimperson") & !str_detect(count_as_filed, "(?i)\\bexploitation|\\bwelfare|\\bfood stamps|\\bbogus|\\bscrap met"),
    bogus_check = str_detect(count_as_filed, "(?i)bogus check"),
    # other property regex -------------------------------------------------------
    burglary = str_detect(count_as_filed, "(?i)burg") & !str_detect(count_as_filed, "(?i)controlled|cds"),
    trespass = str_detect(count_as_filed, "(?i)tres(s|)pass"),
    littering = str_detect(count_as_filed, "(?i)\\blittering|\\bdumping") & !str_detect(count_as_filed, "(?i)\\bburn ban"),
    vandalism = str_detect(count_as_filed, "(?i)\\bvandalism|\\bmolest motor"),
    forged_inst = str_detect(count_as_filed, "(?i)\\bforged inst"),
    computer = str_detect(count_as_filed, "(?i)computer") & !str_detect(count_as_filed, "(?i)\\bthreat|\\bharass|\\bminor|\\babuse|\\bharm|\\blewd"),
    arson = str_detect(count_as_filed, "(?i)arson"),
    regex_property = (
      larceny | receive_stolen | embezzle |
        misc | pawn | hospitality |
        false_pretense | bogus_check | burglary | trespass | littering | vandalism | forged_inst | computer | arson
    ),
    not_property = str_detect(count_as_filed, "(?i)\\bfish|\\blicense|\\b(x|)driv|\\bcds |\\bradio|\\bvirus|\\bcontrolled|\\bexploitation|\\bscreening|\\bidentification|\\btoll|\\bchild|^fourth degree\\bfacts|\\bwelfare|\\bfood stamps|\\bdurg activity|\\bdl |^count|\\belder|\\burine|\\bscrap met|\\bacquire\\b|\\bcds\\b|\\bfood st|\\bplate|\\babuse") & !str_detect(count_as_filed, "(?i)\\bshock fish|\\btag |\\blicense id|\\bfict|\\bfacts"),
    property = regex_property & !not_property
  )

#------------------------------------------------------------------------------

# Ashley vs. Andrew regex ------------------------------------------------------

compare_data <- ashley_data |>
  filter(larceny) |>
  distinct(count_as_filed) |>
  mutate(ashley = T) |>
  full_join(
    final |>
      filter(str_detect(count_as_filed_clean, "(?i)larc")) |>
      distinct(count_as_filed, count_as_filed_clean) |>
      mutate(andrew = T),
    by = c("count_as_filed")
  )

compare_data |>
  count(ashley, andrew, sort = T)

compare_data |>
  filter(is.na(ashley) | is.na(andrew)) |>
  arrange(count_as_filed)

compare_data |>
  filter(is.na(andrew))

#------------------------------------------------------------------------------

# Classification breakdown
final |>
  count(count_as_filed_clean, sort = T) |>
  janitor::adorn_percentages("col") |>
  janitor::adorn_totals() |>
  janitor::adorn_pct_formatting() |>
  janitor::adorn_ns()

# All larceny types
final |>
  filter(str_detect(count_as_filed_clean, "(?i)larc")) |>
  count(count_as_filed_clean, sort = T) |>
  janitor::adorn_totals()

# By case type
final |>
  filter(str_detect(count_as_filed_clean, "Larceny")) |>
  count(count_as_filed_clean,
        case_type,
        year = year(date_filed),
        sort = T) |>
  ggplot(aes(x = year,
             y = n,
             fill = count_as_filed_clean)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~case_type)

# By district
final |>
  filter(str_detect(count_as_filed_clean, "Larceny")) |>
  count(count_as_filed_clean,
        district,
        case_type,
        year = year(date_filed),
        sort = T) |>
  ggplot(aes(x = year,
             y = n,
             fill = count_as_filed_clean)) +
  geom_col() +
  facet_wrap(~district, scales = "free_y")

