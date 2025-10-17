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
        !!!purrr::map2(
          ojoregex::ojo_regex_rules$condition,
          ojoregex::ojo_regex_rules$category,
          \(condition, category) rlang::new_formula(condition, category)
        )
      )
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
