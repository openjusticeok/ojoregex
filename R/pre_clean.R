#' Pre clean charge descriptions to be matched
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
regex_pre_clean <- function(count_as_filed) {

  # Remove "in concert ... [end of string]" (e.g. TAXS, FAIL TO DISPLAY TAX STAMP ON CDS IN CONCERT W/J POOLE)
  clean_text <- str_remove_all(count_as_filed, stringr::regex("\\s*in(| )conc(ert|).*$", ignore_case = TRUE))

  return(clean_text)

}
