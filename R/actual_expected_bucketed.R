#' @title actual_expected_bucketed
#'
#' @description Provides a rank plot of the actual and predicted.
#'
#' @param Target_Variable String of target variable name.
#' @param Model_Object GLM model object.
#' @param Data_Set Training data used when building the model. This is used to plot the number in each class as a barchart if plotly is TRUE.
#' @param plotlyplot Logical if returned plot should be a plotly with number of records as a second axis.
#' @param mywidth Plotly plot width in pixels.
#' @param myheight Plotly plot height in pixels.
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
#' @importFrom plotly "ggplotly"
#' @importFrom plotly "layout"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @import dplyr
#' @import ggplot2
#'


actual_expected_bucketed <- function(Target_Variable, Model_Object, Data_Set, number_of_buckets = 25, plotlyplot = T, mywidth = 800, myheight = 500, Return_data=F, first_colour = 'black', second_colour = '#f185f2'){
  # add defualt to training data if no dataset provided
  # make predictions on data set --------------------------------------------------------------
  # add ability for user to be able to use custom predict function or input a dataset of predictions and actuals
  predicted_dataset <- prettyglm::predict_outcome(target = Target_Variable,
                                                  model_object = Model_Object,
                                                  dataset = Data_Set)

  # tidy data for rank plot --------------------------------------------------------------------
  # Create tidy data to plot
  Plot_data <- dplyr::bind_cols(list(Data_Set, predicted_dataset)) %>% #data.frame(Actual_Values=Actual_Values), data.frame(Predicted_Values=Predicted_Values)))  %>%
    dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) # check if this needs to be made more generic
  Plot_data$Rank <- Plot_data %>%  dplyr::select(Predicted_Values) %>% dplyr::pull() %>% dplyr::ntile(25)
  Plot_data <- Plot_data %>%
    dplyr::mutate(Rank = Rank/25) %>%
    dplyr::rename(Actual = Actual_Values) %>%
    dplyr::rename(Predicted = Predicted_Values) %>%
    tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
    dplyr::group_by(Rank, Data_Type) %>%
    dplyr::summarise(Average_value = mean(value)) %>%
    ungroup()

  # Create plot --------------------------------------------------------------------------------
  p <- Plot_data %>%
    ggplot2::ggplot(aes(x=Rank, y=Average_value, col=Data_Type)) +
    ggplot2::geom_line(size=1) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c(first_colour, second_colour)) + #c('black', '#00d158')) +
    ggplot2::ggtitle("Actual vs Predicted by Predicted Band") +
    ggplot2::xlab('Percentile') +
    ggplot2::ylab('Target') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5),
                   axis.title.x = ggplot2::element_text(vjust=0.5, hjust=0.6, size=12),
                   axis.title.y = ggplot2::element_text(vjust=0.5, hjust=0.5, size=12),
                   #axis.text.x = ggplot2::element_text(angle=90),
                   legend.position="bottom",
                   legend.title=element_blank())

  # Create plotly
  if  (plotlyplot == T){
    p_return <- plotly::ggplotly(p, width = mywidth, height = myheight, dynamicTicks = T, tooltip = c('x', 'y')) %>%
      plotly::layout(autosize=TRUE,
                     legend = list(orientation = "h", y = -0.3, x=0.5, xanchor = "center", title = ''))
  } else{
    p_return <- p
  }

  # Return plot or data ------------------------------------------------------------------------
  if (Return_data == F){
    return(p_return)
  } else{
    return(Plot_data)
  }
}
