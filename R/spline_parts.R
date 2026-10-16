#' @title spline_parts
#'
#' @description Splits spline column names (as created with \code{\link{splineit}}) into the base
#' variable name and the spline min and max, for example `Age_0_25` becomes `Age`, `0` and `25`.
#'
#' @param x Character vector of spline column names.
#' @param spline_seperator String of the spline separator. For example AGE_0_25 would be "_".
#'
#' @return List with elements:
#' \item{variable}{Character vector of base variable names.}
#' \item{min}{Numeric vector of spline minimums, `NA` where `x` is not a spline name.}
#' \item{max}{Numeric vector of spline maximums, `NA` where `x` is not a spline name.}
#'
#' @seealso \code{\link{spline_suffix_regex}}
#'
#' @keywords internal
#'
spline_parts <- function(x, spline_seperator) {
  pattern <- spline_suffix_regex(spline_seperator)
  m <- stringr::str_match(x, pattern)
  base::list(variable = stringr::str_remove(x, pattern),
             min = base::as.numeric(m[, 2]),
             max = base::as.numeric(m[, 3]))
}
