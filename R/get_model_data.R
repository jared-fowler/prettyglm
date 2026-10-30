#' @title get_model_data
#'
#' @description Finds the data to plot for a model, and optionally checks the model's predictors are a type prettyglm can plot.
#' Uses `data_set` if supplied, otherwise the training data stored on the model (`model_object$data` for
#' \code{\link[stats]{glm}}, the `data =` argument of the call for \code{\link[stats]{lm}}).
#'
#' @param model_object \code{\link[stats]{glm}} or \code{\link[stats]{lm}} model object. Can be `NULL` if `data_set` is supplied.
#' @param data_set Optional data frame. If supplied it is returned as is.
#' @param check_predictors Set to TRUE to error on predictor types relativity plots can't handle (character, ordered factor).
#'
#' @return A data frame.
#'
#' @keywords internal
#'
get_model_data <- function(model_object, data_set = NULL, check_predictors = FALSE) {
  # Check the predictor types the plots can't handle ---------------------------------------------
  if (check_predictors && !base::is.null(model_object)) {
    data_classes <- base::attr(stats::terms(model_object), "dataClasses")
    chr_cols <- base::names(data_classes)[data_classes == 'character']
    if (base::length(chr_cols) > 0) {
      base::stop(base::paste0("Character predictors are not supported: ", base::paste(chr_cols, collapse = ", "),
                              ". Convert them to factors before fitting the model, e.g. data$x <- factor(data$x)."))
    }
    ord_cols <- base::names(data_classes)[data_classes == 'ordered']
    if (base::length(ord_cols) > 0) {
      base::stop(base::paste0("Ordered factor predictors are not supported: ", base::paste(ord_cols, collapse = ", "),
                              ". Convert them to unordered factors before fitting the model, e.g. data$x <- factor(data$x, ordered = FALSE)."))
    }
  }

  # Find the data ----------------------------------------------------------------------------------
  if (!base::is.null(data_set)) {
    return(data_set)
  }
  if (base::is.null(model_object)) {
    base::stop("Supply either data_set or model_object.")
  }
  model_data <- model_object$data
  if (!base::is.data.frame(model_data) && !base::is.null(model_object$call$data)) {
    # lm objects don't store data, so evaluate the data = argument of the call
    model_data <- base::tryCatch(
      base::eval(model_object$call$data, base::environment(stats::formula(model_object))),
      error = function(e) NULL
    )
  }
  if (!base::is.data.frame(model_data)) {
    base::stop("Could not find the training data for this model. Fit the model with the data = argument, e.g. glm(y ~ x, data = df), or pass the data in data_set.")
  }
  model_data
}
