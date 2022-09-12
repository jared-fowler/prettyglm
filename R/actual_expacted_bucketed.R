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
#' @importFrom broom "tidy"
#' @importFrom plotly "ggplotly"
#' @importFrom plotly "add_bars"
#' @importFrom plotly "layout"
#' @importFrom tidyselect "all_of"
#' @importFrom lubridate "floor_date"
#' @importFrom tidyr "pivot_longer"
#' @import dplyr
#' @import ggplot2
#'


prediction_rank_plot <- function(Target_Variable, Model_Object, Data_Set, number_of_buckets = 25, plotlyplot = T, mywidth = 800, myheight = 500, Return_data=F){

  # Check if model is a parsnip / tidymodel object --------------------------------------------
  if (any(class(Model_Object) == 'workflow') | any(class(Model_Object) == 'model_fit')){
    parsnip_model <- T
  } else{
    parsnip_model <- F
  }

  # Set up dataset and make predictions --------------------------------------------------------
  # Make sure data is a dataframe not a tibble
  Data_Set <- as.data.frame(Data_Set)

  # Make approprite predictions
  Actual_Values <- dplyr::pull(dplyr::select(Data_Set, tidyselect::all_of(c(Target_Variable))))
  if(class(Actual_Values) == 'factor'){
    Actual_Values <- base::as.numeric(as.character(Actual_Values))
  }

  # if object is a parsnip, then make prediction that way
  if (parsnip_model == T){
    if (Model_Object$fit$actions$model$spec$mode == "classification"){
      Predicted_Values <- dplyr::pull(dplyr::select(predict(object = Model_Object, new_data = Data_Set, type='prob'), '.pred_1'))
    } else{
      Predicted_Values <- dplyr::pull(predict(Model_Object, dplyr::select(Data_Set, -c(Target_Variable)), type='numeric'))
    }
  } else{
    Predicted_Values <- base::as.numeric(stats::predict(Model_Object, Data_Set, type='response'))
  }
  Residual_Values <- ((base::as.numeric(Actual_Values))-Predicted_Values)

  # tidy data for rank plot --------------------------------------------------------------------
  # Create tidy data to plot
  Plot_data <- dplyr::bind_cols(list(Data_Set, data.frame(Actual_Values=Actual_Values), data.frame(Predicted_Values=Predicted_Values)))  %>%
    dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) # check if this needs to be made more generic
  Plot_data$Rank <- Plot_data %>%  dplyr::select(Predicted_Values) %>% dplyr::pull() %>% dplyr::ntile(25)
  Plot_data <- Plot_data %>%
    dplyr::mutate(Rank = Rank/25) %>%
    tidyr::pivot_longer(c(Actual_Values, Predicted_Values), names_to = 'Data_Type', values_to = 'value') %>%
    dplyr::group_by(Rank, Data_Type) %>%
    dplyr::summarise(Average_value = mean(value)) %>%
    ungroup()

  # Create plot --------------------------------------------------------------------------------
  p <- Plot_data %>%
    ggplot2::ggplot(aes(x=Rank, y=Average_value, col=Data_Type)) +
    ggplot2::geom_line(size=1) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c('black', '#00d158')) +
    ggplot2::ggtitle("Actual vs Predicted by Predicted Band") +
    ggplot2::xlab('Percentile') +
    ggplot2::ylab('Target') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5),
                   axis.title.x = ggplot2::element_text(vjust=0.5, hjust=0.6, size=12),
                   axis.title.y = ggplot2::element_text(vjust=0.5, hjust=0.5, size=12),
                   #axis.text.x = ggplot2::element_text(angle=90),
                   legend.position="bottom",
                   legend.title = ggplot2::element_blank())

  # Create plotly
  if  (plotlyplot == T){
    p_return <- plotly::ggplotly(p, width = mywidth, height = myheight, dynamicTicks = T, tooltip = c('x', 'y')) %>%
      plotly::layout(autosize=TRUE,
                     legend = list(orientation = "h", y = -0.3, x=0.5, xanchor = "center"))
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
