#' Pre clean charge descriptions to be matched
#'
#' This function pre-cleans charge descriptions to be matched by removing specific patterns that are not relevant for matching. It removes phrases like "in concert with" from the end of the charge descriptions.
#'
#' @param count_as_filed A character vector containing the charge descriptions to be pre-cleaned.
#'
#' @return A character vector with pre-cleaned charge descriptions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' clean_text <- regex_pre_clean("TAXS, FAIL TO DISPLAY TAX STAMP ON CDS IN CONCERT W/J POOLE")
#' clean_text
#' }
regex_pre_clean <- function(count_as_filed) {

  # Remove "in concert ... [end of string]"
  clean_text <- stringr::str_remove_all(count_as_filed, stringr::regex("\\s*in(| )conc(ert|).*$", ignore_case = TRUE))

  return(clean_text)

}
