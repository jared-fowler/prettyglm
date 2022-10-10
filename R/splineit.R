#' @title splineit
#'
#' @description Splines a continous variable
#'
#' @param var Continous vector to spline.
#' @param min Min of spline.
#' @param max Max of spline.
#'
#' @return
#' Splined Column
#'
#' @examples
#' # add an example here
#' @export
#'
splineit <- function(var,min,max) {
  x = base::ifelse(var-min < 0 , 0, var-min)
  x = base::ifelse(var-min >= max-min , max-min , x)
  return (x)
}
