#' Return OJO Regex for a given flag
#'
#' This function returns the regex string for a given flag
#'
#' @param flag The flag you want to get the regex pattern for
#'
#' @return A string of regex
#'
#' @export
#'
ojo_get_flag_regex <- function(flag = NA) {
  if (!flag %in% ojo_regex_flags$flag) {
    # rlang::arg_match(arg = flag, values = ojo_regex_flags$flag)
    rlang::abort("That flag was not found in the list.")
  }

  regex <- ojoregex::ojo_regex_flags |>
    dplyr::filter(flag == {{ flag }}) |>
    dplyr::pull(var = regex)

  # Case insensitive
  paste0("(?i)", regex)
}

#' Return OJO Regex for a given statute
#'
#' This function returns the regex string for a given statute
#'
#' @param statute The statute you want to get the regex pattern for
#'
#' @return A string of regex
#'
#' @export
#'
ojo_get_statute_regex <- function(statute = NA) {
  # This will require adding the actual regex flag combos into the sheet somehow
}
