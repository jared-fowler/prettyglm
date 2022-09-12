#' @title one_way_ave
#'
#' @description Creates a pretty html plot of one way actual vs expected by specified predictor.
#'
#' @param feature_to_plot A string of the variable to plot.
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats]{glm}, \link[stats]{lm},  \link[parsnip]{linear_reg} or \link[parsnip]{logistic_reg}
#' @param target_variable String of target variable name in dataset.
#' @param data_set Data set to calculate the actual vs expected for. If no input default is to try and extract training data from model object.
#' @param plot_factor_as_numeric Set to TRUE to return \link[base]{data.frame} instead of creating \link[knitr]{kable}.
#' @param ordering Option to change the ordering of categories on the x axis, only for discrete categories. Default to the ordering of the factor. Other options are: 'alphabetical', 'Number of records', 'Average Value'
#' @param width Width of plot
#' @param height Height of plot
#' @param return_data Set to TRUE to return data set instead of plot
#'
#' @return plotly plot of one_way_ave. \link[base]{data.frame} if return_data = TRUE.
#'
#' @examples
#' library(dplyr)
#' library(prettyglm)
#' data('titanic')
#' columns_to_factor <- c('Pclass',
#'                        'Sex',
#'                        'Cabin',
#'                        'Embarked',
#'                        'Cabintype',
#'                        'Survived')
#' titanic  <- titanic  %>%
#'   dplyr::mutate_at(columns_to_factor, list(~factor(.)))
#' # Fit a model without Pclass to see ave.
#' survival_model <- stats::glm(Survived ~
#'                               #Pclass +
#'                               Sex +
#'                               Age +
#'                               Fare +
#'                               Embarked +
#'                               SibSp +
#'                               Parch +
#'                               Cabintype,
#'                              data = titanic,
#'                              family = binomial(link = 'logit'))
#'
#' @export
#' @importFrom tibble "tibble"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @import dplyr
#' @import plotly
#'

one_way_ave <- function(feature_to_plot, model_object, target_variable, data_set, plot_factor_as_numeric, ordering, width, height, return_data){

  # Extract the actual and expected values -------------------------------------------


  # tidy the data for processing -----------------------------------------------------

  # maybe make rule that if it is continuous we bucket it....



  # change the ordering if specified -------------------------------------------------


}









