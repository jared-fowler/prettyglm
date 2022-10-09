#' @title actual_expected_bucketed
#'
#' @description Provides a rank plot of the actual and predicted.
#'
#' @param target_variable String of target variable name.
#' @param model_object GLM model object.
#' @param data_set Training data used when building the model. This is used to plot the number in each class as a barchart if plotly is TRUE.
#' @param width Plotly plot width in pixels.
#' @param height Plotly plot height in pixels.
#' @param Return_data Logical to return cleaned dataset instead of plot.
#' @param first_colour First colour to plot, usually the colour of actual.
#' @param second_colour Second colour to plot, usually the colour of predicted.
#'

#' @return plot
#' Plotly plot by defualt.
#' ggplot if plotlyplot = F.
#' Tibble if Return_data = T.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#' @importFrom tibble "tibble"
#' @importFrom plotly "layout"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @import dplyr
#'


actual_expected_bucketed <- function(target_variable, model_object, data_set = NULL, number_of_buckets = 25, width = 800, height = 500, Return_data=F, first_colour = 'black', second_colour = '#cc4678', predict_function = NULL){
  # remove gg plot references
  # tidy help and frist three inputs
  # add ability to facet???
  # add ability for user to be able to use custom predict function or input a dataset of predictions and actuals

  # make predictions on data set --------------------------------------------------------------

  # if provided dataset is null then use the training data from model object
  if (is.null(data_set)==T){
    # Extract training data from model object
    if (base::any(class(model_object) == 'workflow')){
      # Workflow model objects here
      data_set <- tidy_workflow$fit$fit$fit$data
    } else if(base::any(class(model_object) == 'model_fit')){
      # pasnip model objects here
      data_set <- model_object$fit$data
    } else{
      #stats::glm objects here
      data_set <- model_object$data
    }
  }

  # make predictions
  if (is.null(predict_function) == T){
    predicted_dataset <- prettyglm::predict_outcome(target = target_variable,
                                                    model_object = model_object,
                                                    dataset = data_set)
  } else{
    base::simpleError('Functionality for custom predict function not avaliable yet')
  }

  # tidy data for rank plot --------------------------------------------------------------------
  # Create tidy data to plot
  Plot_data <- dplyr::bind_cols(list(data_set, predicted_dataset)) %>%
    dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) # check if this needs to be made more generic
  Plot_data$Rank <- Plot_data %>%  dplyr::select(Predicted_Values) %>% dplyr::pull() %>% dplyr::ntile(25)
  Plot_data <- Plot_data %>%
    dplyr::mutate(Rank = Rank/25) %>%
    dplyr::rename(Actual = Actual_Values) %>%
    dplyr::rename(Predicted = Predicted_Values) %>%
    tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
    dplyr::group_by(Rank, Data_Type) %>%
    dplyr::summarise(Average_value = mean(value), .groups = "drop") %>%
    ungroup()

  # Create plot --------------------------------------------------------------------------------
  p_return <- plotly::plot_ly(Plot_data,
                              height = height,
                              width = width,
                              colors = c(first_colour, second_colour)) %>%
    plotly::add_trace(x = ~Rank,
                      y = ~Average_value,
                      type="scatter",
                      mode="lines",
                      color = ~Data_Type,
                      line = list(width = 4),
                      yaxis = "y2") %>%
    plotly::layout(yaxis2 = list(side = 'left',
                                 title = 'Target',
                                 showgrid = T,
                                 zeroline = FALSE,
                                 overlaying = 'y'),
                   legend = list(orientation = "h",
                                 y = -0.2,
                                 x = 0.37,
                                 title = ''),
                   xaxis = list(title = 'Percentile',
                                zeroline = FALSE),
                   title = "Actual vs Predicted by Predicted Band",
                   autosize = T,
                   margin = list(b = 50, l = 50, r=80))

  # Return plot or data ------------------------------------------------------------------------
  if (Return_data == F){
    return(p_return)
  } else{
    return(Plot_data)
  }
}
