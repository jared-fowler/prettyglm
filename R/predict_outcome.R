#' @title predict_outcome
#'
#' @description Processing to predict response for various actual vs expected plots
#'
#' @param target String of target variable name.
#' @param model_object Model object. prettyglm currently supports
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
#'

predict_outcome <- function(target, model_object, dataset, prediction_type = NULL, weights = NULL){

  # Check if model is a parsnip / tidymodel object --------------------------------------------
  if (base::any(base::class(model_object) == 'workflow') | base::any(base::class(model_object) == 'model_fit')){
    parsnip_model <- T
    # if a parsnip model, set the model mode
    if (model_object$fit$actions$model$spec$mode == 'classification'){
      model_mode <- 'classification'
    } else{
      model_mode <- 'regression'
    }
  } else{
    parsnip_model <- F
  }

  # If prediction_type is NULL, set a sensible default ----------------------------------------
  # if parsnip and
  if (is.null(prediction_type) == T){
    if (parsnip_model == T & model_mode == 'classification'){
      prediction_type <- 'prob'
    } else if(parsnip_model == T & model_mode == 'classification'){
      prediction_type <- 'numeric'
    } else if(parsnip_model == F){
      print("Warning: No prediction_type set, defaulting to 'response'")
      prediction_type <- 'response'
    }
  }

  # Extract actual values ---------------------------------------------------------------------
  # Make sure dataset is a dataframe not a tibble
  dataset <- base::as.data.frame(dataset)
  Actual_Values <- dplyr::pull(dplyr::select(dataset, tidyselect::all_of(c(Target_Variable))))
  if(class(Actual_Values) == 'factor'){
    Actual_Values <- base::as.numeric(as.character(Actual_Values))
  }

  # Make predictions --------------------------------------------------------------------------
  # if object is a parsnip, then make prediction that way
  if (parsnip_model == T){
    if (model_mode == "classification"){
      Predicted_Values <- dplyr::pull(dplyr::select(predict(object = model_object, new_data = dataset, type=prediction_type), '.pred_1'))
    } else{
      Predicted_Values <- dplyr::pull(predict(model_object, dplyr::select(dataset, -c(Target_Variable)), type=prediction_type))
    }
  } else{
    Predicted_Values <- base::as.numeric(stats::predict(model_object, dataset, type=prediction_type))
  }
  Residual_Values <- ((base::as.numeric(Actual_Values))-Predicted_Values)


  # Return a dataframe of actual and predicted values ------------------------------------------
  return(base::data.frame(Actual = Actual_Values,
                          Predicted = Predicted_Values))
}
