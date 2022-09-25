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
#' @return plotly plot of one way actual vs expected \link[base]{data.frame} if return_data = TRUE.
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

one_way_ave <- function(feature_to_plot, model_object, target_variable, data_set, Plot_Type = 'predictions', plot_factor_as_numeric = FALSE, ordering = NULL, plotlyplot = TRUE, mywidth = 800, myheight = 500, Return_data=F, predict_function = NULL, number_of_buckets = NULL){

  # add ability for user to be able to use custom predict function or input a dataset of predictions and actuals
  # make sure to document

  # Extract the actual and expected values -------------------------------------------
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

  # tidy data for plotting different if factor or continuous -----------------------------------
  Plot_data <- dplyr::bind_cols(list(data_set, predicted_dataset))
  if (class(dplyr::select(Plot_data, tidyselect::matches(feature_to_plot))[[1]]) %in% c('factor','integer','character')){
    # categorical logic
    if (base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0) Plot_data <- Plot_data[-base::which(base::is.na(Plot_data$Predicted_Values)),]
    Plot_data <- Plot_data %>%
      dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
      dplyr::rename(Actual = Actual_Values) %>%
      dplyr::rename(Predicted = Predicted_Values) %>%
      tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
      dplyr::group_by_at(c(feature_to_plot,'Data_Type')) %>%
      dplyr::summarise(Average_value = mean(value),
                       Number_of_Records = n(),
                       .groups = 'drop') %>%
      dplyr::ungroup()

    # Create plots --------------------------------------------------------------------------------
    if (Plot_Type == 'residuals'){
      Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual_Values')
      ylabeltext <- 'Residual'
      Plottitle <- paste('Residuals for',feature_to_plot)
    } else if (Plot_Type == 'predictions'){
      Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type != 'Residual_Values')
      ylabeltext <- target_variable
      Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
    } else if (Plot_Type == 'actuals'){
      Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual_Values')
      ylabeltext <- target_variable
      Plottitle <- paste('Actual for',feature_to_plot)
    } else{
      print("Plot_Type must be one of: 'residuals', 'predictions' or 'actuals'")
    }

    if (plot_factor_as_numeric ==TRUE){
      Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(feature_to_plot)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(feature_to_plot))))))
    }

    # Change ordering if specified -----------------------------------------------------------------
    if (base::is.null(ordering) ==  F){
      order_option <- ordering
      if (length(order_option) >1){
        Plot_data_to_plot <- Plot_data_to_plot %>%
          dplyr::mutate_at(.vars = c(feature_to_plot), .funs = ~base::factor(. , levels = order_option))
      } else{
        if (ordering == 'alphabetical'){
          order_option <-  feature_to_plot
        } else if (ordering == 'Number of records'){
          order_option <- 'Number_of_Records'
        } else if (ordering == 'Average Value'){
          order_option <- 'Average_value'
        } else{
          base::print('You have entered an incorrect ordering option. Please enter: alphabetical, Number of records, Average Value or a vector of level names')
        }
        Plot_data_to_plot <- Plot_data_to_plot %>%
          dplyr::arrange(get(order_option)) %>%
          dplyr::mutate_at(.vars = c(feature_to_plot), .funs = ~base::factor(., base::unique(.)))
      }
    }

    # Create plot ----------------------------------------------------------------------------------
    p <- Plot_data_to_plot %>%
      ggplot2::ggplot(ggplot2::aes_string(x=feature_to_plot, y='Average_value', col='Data_Type', group='Data_Type', shape='Data_Type')) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_manual(values = c('black', '#00d158')) +
      ggplot2::scale_shape_manual(values = c(15, 19)) +
      #scale_color_manual(values=c(viridis(2, alpha = 1, begin = 0.3, end = 0.7, option = "D"))) +
      ggplot2::ggtitle(Plottitle) +
      ggplot2::xlab(feature_to_plot) +
      ggplot2::ylab(ylabeltext) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5),
                     axis.title.x = ggplot2::element_text(vjust=0.5, hjust=0.5, size=12),
                     axis.title.y = ggplot2::element_text(vjust=0.5, hjust=0.5, size=12),
                     axis.text.x = ggplot2::element_text(angle=90),
                     legend.position="bottom",
                     legend.title = ggplot2::element_blank())

    if (plotlyplot == TRUE){
      Count_data <- Plot_data_to_plot %>%
        dplyr::select(c(feature_to_plot, 'Number_of_Records')) %>%
        base::unique()

      p_return <- plotly::ggplotly(p, width = mywidth, height = myheight, dynamicTicks = T, tooltip=c('x','y')) %>%
        plotly::add_bars(data = Count_data,
                         x=dplyr::pull(dplyr::select(Count_data, tidyselect::all_of(feature_to_plot))),
                         y=dplyr::pull(dplyr::select(Count_data, tidyselect::all_of('Number_of_Records'))),
                         name= 'Number of records',
                         yaxis = 'y2',
                         marker = list(color = '#dddddd',
                                       line = list(width=0,
                                                   color='black'))) %>%
        plotly::layout(yaxis2 = list(side = 'right',
                                     title = 'Number of Records',
                                     showgrid = F),
                       yaxis = list(overlaying='y2',
                                    side = 'left',
                                    showgrid = T),
                       legend = list(orientation = "h",
                                     xanchor = "center",
                                     x=0.5,
                                     y=-0.35,
                                     title = ''),
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
    } else{
      p_return <- p
    }
  } else{
    # continuous logic -------------------------------------------------------------------------
    if (base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0) Plot_data <- Plot_data[-base::which(base::is.na(Plot_data$Predicted_Values)),]
    # default number of buckets for continuous variables is 30
    if(base::is.null(number_of_buckets) == T){
      number_of_buckets <- 30
    }
    # prep the data
    Plot_data[,base::paste0(feature_to_plot,'_cat')] = Hmisc::cut2(dplyr::pull(dplyr::select(Plot_data, feature_to_plot)), g = number_of_buckets, levels.mean = T)
    Plot_data <- Plot_data %>%
      dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
      dplyr::rename(Actual = Actual_Values) %>%
      dplyr::rename(Predicted = Predicted_Values) %>%
      tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
      dplyr::group_by_at(c(base::paste0(feature_to_plot,'_cat'),'Data_Type')) %>%
      dplyr::summarise(Average_value = mean(value),
                       Number_of_Records = n(),
                       .groups = 'drop') %>%
      dplyr::ungroup()

    if (Plot_Type == 'residuals'){
      Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual_Values')
      ylabeltext <- 'Residual'
      Plottitle <- paste('Residuals for',feature_to_plot)
    } else if (Plot_Type == 'predictions'){
      Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type != 'Residual_Values')
      ylabeltext <- target_variable
      Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
    } else if (Plot_Type == 'actuals'){
      Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual_Values')
      ylabeltext <- target_variable
      Plottitle <- paste('Actual for',feature_to_plot)
    } else{
      print("Plot_Type must be one of: 'residuals', 'predictions' or 'actuals'")
    }

    # plot factor as numeric is by default true
    Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(base::paste0(feature_to_plot,'_cat'))), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(base::paste0(feature_to_plot,'_cat')))))))

    # Create plot ----------------------------------------------------------------------------------
    p <- Plot_data_to_plot %>%
      ggplot2::ggplot(ggplot2::aes_string(x=base::paste0(feature_to_plot,'_cat'), y='Average_value', col='Data_Type', group='Data_Type', shape='Data_Type')) +
      ggplot2::geom_point(size=2) +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_manual(values = c('black', '#00d158')) +
      ggplot2::scale_shape_manual(values = c(15, 19)) +
      #scale_color_manual(values=c(viridis(2, alpha = 1, begin = 0.3, end = 0.7, option = "D"))) +
      ggplot2::ggtitle(Plottitle) +
      ggplot2::xlab(feature_to_plot) +
      ggplot2::ylab(ylabeltext) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5),
                     axis.title.x = ggplot2::element_text(vjust=0.5, hjust=0.5, size=12),
                     axis.title.y = ggplot2::element_text(vjust=0.5, hjust=0.5, size=12),
                     axis.text.x = ggplot2::element_text(angle=90),
                     legend.position="bottom",
                     legend.title = ggplot2::element_blank())

    if (plotlyplot == TRUE){
      fit <- density(dplyr::pull(dplyr::select(dplyr::bind_cols(list(data_set, predicted_dataset)), all_of(feature_to_plot))))
      p_return <- plotly::ggplotly(p, width = mywidth, height = myheight, dynamicTicks = T, tooltip=c('x','y')) %>%
        plotly::add_trace(x = fit$x,
                          y = fit$y,
                          type = "scatter",
                          mode = "lines",
                          fill = "tozeroy",
                          yaxis = "y2",
                          name = "Density",
                          fillcolor = 'rgba(221,221,221,0.5)',
                          line  = list(color = 'rgba(221,221,221,0.7)')) %>%
        #marker = list(color = 'rgba(221,221,221,0.5)')) %>%
        plotly::layout(yaxis = list(side = 'left',
                                    showgrid = T),
                       yaxis2 = list(overlaying = 'y',
                                     side = 'right',
                                     title = 'Density',
                                     showgrid = F),
                       legend = list(orientation = "h",
                                     y = -0.35,
                                     x = 0.2,
                                     title = ''),
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
    } else{
      p_return <- p
    }
  }
  return(p_return)
}









