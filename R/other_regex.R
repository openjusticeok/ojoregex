#' Unhoused / Homeless address regex
#'
#' Detects whether an address is likely to indicate "homeless" / "unhoused" / "NA", etc.
#'
#' @export
ojo_regex_unhoused <- c(
  "(?i)trans|homele|\\b(^0*$)\\b|transint|trancient|unknown|tranisent|tracient|tranient|^na$"
)
