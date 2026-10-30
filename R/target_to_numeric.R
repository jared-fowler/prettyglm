#' @title target_to_numeric
#'
#' @description Converts a target variable to numeric so actuals can be averaged.
#' Factors with numeric levels (e.g. "0"/"1") keep their values. A two level factor with non-numeric levels
#' (e.g. "no"/"yes") is coded 0/1 the same way \code{\link[stats]{glm}} codes it (second level is 1).
#'
#' @param y Target vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#'
target_to_numeric <- function(y) {
  if (base::is.logical(y)) {
    return(base::as.numeric(y))
  }
  if (!base::is.factor(y) && !base::is.character(y)) {
    return(y)
  }
  y <- base::factor(y)
  y_numeric <- base::suppressWarnings(base::as.numeric(base::as.character(y)))
  if (!base::any(base::is.na(y_numeric) & !base::is.na(y))) {
    return(y_numeric)
  }
  if (base::nlevels(y) == 2) {
    base::warning(base::paste0("Target is a factor, coding '", base::levels(y)[2], "' as 1 and '",
                               base::levels(y)[1], "' as 0 (same as glm)."))
    return(base::as.integer(y) - 1)
  }
  base::stop("Target must be numeric or a two level factor.")
}
