#' @title predict_outcome
#'
#' @description Processing to predict response for various actual vs expected plots
#'
#' @param target String of target variable name.
#' @param model_object \code{\link[stats]{glm}} or \code{\link[stats]{lm}} model object.
#' @param dataset This is used to plot the number in each class as a barchart if plotly is TRUE.
#' @param prediction_type type of prediction to be passed to the model object. For ...GLM defaults to ....
#' @param weights weightings to be provided to predictions if required.
#'
#' @return
#' \item{dataframe}{Returns a dataframe of Actual and Predicted Values}
#'
#' @seealso \code{\link[broom]{tidy.lm}}
#'
#' @author Jared Fowler
#'
#' @export
#' @import dplyr
#' @importFrom methods "is"
#'

predict_outcome <- function(target, model_object, dataset, prediction_type = NULL, weights = NULL){

  # # Allow for global variables
  # utils::globalVariables(c("predict"))

  # If prediction_type is NULL, set a sensible default ----------------------------------------
  if (is.null(prediction_type) == T){
    warning("Warning: No prediction_type set, defaulting to 'response'")
    prediction_type <- 'response'
  }

  # Extract actual values ---------------------------------------------------------------------
  # Make sure dataset is a dataframe not a tibble
  dataset <- base::as.data.frame(dataset)
  Actual_Values <- dplyr::pull(dplyr::select(dataset, tidyselect::all_of(c(target))))
  if(methods::is(Actual_Values,'factor') == TRUE){
    Actual_Values <- base::as.numeric(as.character(Actual_Values))
  }

  # Make predictions --------------------------------------------------------------------------
  Predicted_Values <- base::as.numeric(stats::predict(model_object, dataset, type=prediction_type))
  Residual_Values <- ((base::as.numeric(Actual_Values))-Predicted_Values)


  # Return a dataframe of actual and predicted values ------------------------------------------
  return(base::data.frame(Actual_Values = Actual_Values,
                          Predicted_Values = Predicted_Values))
}
