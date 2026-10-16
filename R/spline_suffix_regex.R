#' @title spline_suffix_regex
#'
#' @description Builds a regular expression matching the `<sep><min><sep><max>` suffix of a spline
#' column name, for example `_0_25` in `Age_0_25`. The pattern is anchored to the end of the string so
#' the base variable name may itself contain the separator (e.g. `Fare_paid_0_250`).
#'
#' @param spline_seperator String of the spline separator. For example AGE_0_25 would be "_".
#' Regex special characters are escaped, so separators such as "." are matched literally.
#'
#' @return String regular expression with two capture groups: the spline min and max.
#'
#' @keywords internal
#'
spline_suffix_regex <- function(spline_seperator) {
  sep <- base::gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", spline_seperator)
  base::paste0(sep, "(-?[0-9.]+)", sep, "(-?[0-9.]+)$")
}
