#' @title one_way_ave
#'
#' @description Creates a pretty html plot of one way actual vs expected by specified predictor.
#'
#' @param feature_to_plot A string of the variable to plot.
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats]{glm}, \link[stats]{lm},  \link[parsnip]{linear_reg} or \link[parsnip]{logistic_reg}
#' @param target_variable String of target variable name in dataset.
#' @param data_set Data set to calculate the actual vs expected for. If no input default is to try and extract training data from model object.
#' @param plot_type one of "Residual", "predictions" or "actuals" defaults to "predictions"
#' @param plot_factor_as_numeric Set to TRUE to return \link[base]{data.frame} instead of creating \link[knitr]{kable}.
#' @param ordering Option to change the ordering of categories on the x axis, only for discrete categories. Default to the ordering of the factor. Other options are: 'alphabetical', 'Number of records', 'Average Value'
#' @param width Width of plot
#' @param height Height of plot
#' @param number_of_buckets Number of buckets for continuous variable plots
#' @param first_colour First colour to plot, usually the colour of actual.
#' @param second_colour Second colour to plot, usually the colour of predicted.
#' @param facetby Variable to facet the actual vs expect plots by.
#' @param predict_function to use. Still in development.
#' @param upper_percentile_to_cut For continuous variables this is what percentile to exclude from the upper end of the distribution. Defaults to 0.01, so the maximum percentile of the variable in the plot will be 0.99. Cutting off some of the distribution can help the views if outlier's are present in the data.
#' @param lower_percentile_to_cut For continuous variables this is what percentile to exclude from the lower end of the distribution. Defaults to 0.01, so the mimimum percentile of the variable in the plot will be 0.01. Cutting off some of the distribution can help the views if outlier's are present in the data.
#'
#' @return plotly plot of one way actual vs expected.
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
#' @importFrom stats "density"
#' @importFrom stats "predict"
#' @import dplyr
#' @import plotly
#'

one_way_ave <- function(feature_to_plot, model_object, target_variable, data_set, plot_type = 'predictions', plot_factor_as_numeric = FALSE, ordering = NULL, width = 800, height = 500, number_of_buckets = NULL, first_colour = 'black', second_colour = '#cc4678', facetby = NULL, predict_function = NULL, upper_percentile_to_cut = 0, lower_percentile_to_cut = 0){
  # add ability for user to be able to use custom predict function or input a dataset of predictions and actuals
  # make sure to document
  # better value / target label / maybe user can choose????????
  # clean non - faceted code to also be in base plotly.... maybe work as a goal to remove dependancy on ggplot2 and make pacakges entirely plotly supported
  # Make sure plots can handle residuals as a plot_type input

  # Clean all code, update exmaples to include some interactions

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

    if (is.null(facetby)==F){
      # faceted code ---------------------------------------------------------------------------
      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby)))))
      l <- 1
      plotlist <- list()
      while (l <= numberoffacets){
        facettoplot <- base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby))))[l]

        # prep the data--------------------------------------------------------------
        Plot_data_inside <- Plot_data %>%
          dplyr::filter(get(facetby) == facettoplot) %>%
          dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
          dplyr::rename(Actual = Actual_Values) %>%
          dplyr::rename(Predicted = Predicted_Values) %>%
          tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
          dplyr::group_by_at(c(feature_to_plot,'Data_Type')) %>%
          dplyr::summarise(Average_value = mean(value),
                           Number_of_Records = n(),
                           .groups = 'drop') %>%
          dplyr::ungroup()

        if (plot_type == 'residuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Residual_Values')
          ylabeltext <- 'Residual'
          Plottitle <- paste('Residuals for',feature_to_plot)
        } else if (plot_type == 'predictions'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type != 'Residual_Values')
          ylabeltext <- target_variable
          Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
        } else if (plot_type == 'actuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Actual_Values')
          ylabeltext <- target_variable
          Plottitle <- paste('Actual for',feature_to_plot)
        } else{
          print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
        }

        if (plot_factor_as_numeric ==TRUE){
          Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(feature_to_plot)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(feature_to_plot))))))
        }

        Count_data <- Plot_data_to_plot %>%
          dplyr::select(c(feature_to_plot, 'Number_of_Records')) %>%
          base::unique()

        plotlist[[l]] <- Plot_data_to_plot %>%
          plotly::plot_ly(colors = c(first_colour, second_colour),
                          height = height,
                          width = width,
                          showlegend = base::ifelse(l==1,T,F)) %>%
          plotly::add_trace(x = ~get(feature_to_plot),
                            y = ~Average_value,
                            type="scatter",
                            mode="lines+markers",
                            color = ~Data_Type,
                            yaxis = "y2") %>%
          plotly::add_bars(data = Count_data,
                           x=dplyr::pull(dplyr::select(Count_data, tidyselect::all_of(feature_to_plot))),
                           y=dplyr::pull(dplyr::select(Count_data, tidyselect::all_of('Number_of_Records'))),
                           name= 'Number of records',
                           yaxis = 'y',
                           marker = list(color = '#dddddd',
                                         line = list(width=0,
                                                     color='black'))) %>%
          plotly::layout(yaxis = list(side = 'right',
                                      title = 'Number of Recrods'),
                         yaxis2 = list(side = 'left',
                                       title = 'Target',
                                       showgrid = F,
                                       overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                         legend = list(orientation = "h",
                                       y = -0.2,
                                       x = 0.32,
                                       title = ''),
                         xaxis = list(title = feature_to_plot),
                         title = Plottitle,
                         autosize = T,
                         margin = list(b = 50, l = 50, r=80)) %>%
          plotly::add_annotations(
            x= 0.5,
            y= 1.05,
            xref = "paper",
            yref = "paper",
            text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
            showarrow = F
          )
        l <- l + 1
      }
      p_return <- plotly::subplot(plotlist,
                                  nrows = numberoffacets,
                                  titleY = T,
                                  titleX = T,
                                  margin = 0.07,
                                  shareY = F,
                                  shareX = T)
    } else{
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
      if (plot_type == 'residuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual_Values')
        ylabeltext <- 'Residual'
        Plottitle <- paste('Residuals for',feature_to_plot)
      } else if (plot_type == 'predictions'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type != 'Residual_Values')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
      } else if (plot_type == 'actuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual_Values')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual for',feature_to_plot)
      } else{
        print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
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

      # Create plot
      p_return <- Plot_data_to_plot %>%
        plotly::plot_ly(colors = c(first_colour, second_colour),
                        height = height,
                        width = width,
                        showlegend = T) %>%
        plotly::add_trace(x = ~get(feature_to_plot),
                          y = ~Average_value,
                          type="scatter",
                          mode="lines+markers",
                          color = ~Data_Type,
                          yaxis = "y2") %>%
        plotly::add_bars(x=~get(feature_to_plot),
                         y=~Number_of_Records,
                         name= 'Number of records',
                         yaxis = 'y',
                         marker = list(color = '#dddddd',
                                       line = list(width=0,
                                                   color='black'))) %>%
        plotly::layout(yaxis = list(side = 'right',
                                    title = 'Number of Recrods'),
                       yaxis2 = list(side = 'left',
                                     title = 'Target',
                                     showgrid = F,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.3,
                                     title = ''),
                       xaxis = list(title = feature_to_plot),
                       title = Plottitle,
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  } else{
    # continuous logic  -------------------------------------------------------------------------
    if (base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0) Plot_data <- Plot_data[-base::which(base::is.na(Plot_data$Predicted_Values)),]
    # default number of buckets for continuous variables is 30
    if(base::is.null(number_of_buckets) == T){
      number_of_buckets <- 30
    }
    # prep the data and cut the data
    Plot_data <- Plot_data%>%
      dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
      dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T)))

    Plot_data[,base::paste0(feature_to_plot,'_cat')] = prettyglm::cut3(x = dplyr::pull(dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot))), g = number_of_buckets)
    if (is.null(facetby)==F){
      # faceted code ---------------------------------------------------------------------------
      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby)))))
      l <- 1
      plotlist <- list()
      while (l <= numberoffacets){
        facettoplot <- base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby))))[l]

        # prep the data
        Plot_data_inside <- Plot_data %>%
          dplyr::filter(get(facetby) == facettoplot) %>%
          dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
          dplyr::rename(Actual = Actual_Values) %>%
          dplyr::rename(Predicted = Predicted_Values) %>%
          tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
          dplyr::group_by_at(c(base::paste0(feature_to_plot,'_cat'),'Data_Type')) %>%
          dplyr::summarise(Average_value = mean(value),
                           Number_of_Records = n(),
                           .groups = 'drop') %>%
          dplyr::ungroup()

        if (plot_type == 'residuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Residual_Values')
          ylabeltext <- 'Residual'
          Plottitle <- paste('Residuals for',feature_to_plot)
        } else if (plot_type == 'predictions'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type != 'Residual_Values')
          ylabeltext <- target_variable
          Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
        } else if (plot_type == 'actuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Actual_Values')
          ylabeltext <- target_variable
          Plottitle <- paste('Actual for',feature_to_plot)
        } else{
          print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
        }

        # plot factor as numeric is by default true
        Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(base::paste0(feature_to_plot,'_cat'))), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(base::paste0(feature_to_plot,'_cat')))))))

        # try the plotly plot!!!!!! --------------------------------------------------------------------
        density_data <- dplyr::bind_cols(list(data_set, predicted_dataset)) %>%
          dplyr::filter(get(facetby) == facettoplot)

        fit <- density_data %>%
          dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
          dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
          dplyr::select(tidyselect::all_of(feature_to_plot)) %>%
          dplyr::pull() %>%
          stats::density(.,na.rm = T)

        plotlist[[l]] <- Plot_data_to_plot %>%
          plotly::plot_ly(colors = c(first_colour, second_colour),
                          height = height,
                          width = width,
                          showlegend = base::ifelse(l==1,T,F)) %>%
          plotly::add_trace(x = ~get(base::paste0(feature_to_plot,'_cat')),
                            y = ~Average_value,
                            type="scatter",
                            mode="lines+markers",
                            color = ~Data_Type,
                            yaxis = "y2") %>%
          plotly::add_trace(x = fit$x,
                            y = fit$y,
                            type = "scatter",
                            mode = "lines",
                            fill = "tozeroy",
                            yaxis = "y",
                            name = "Density",
                            fillcolor = 'rgba(221,221,221,0.5)',
                            line  = list(color = 'rgba(221,221,221,0.7)')) %>%
          plotly::layout(yaxis = list(side = 'right',
                                      title = 'Density'),
                         yaxis2 = list(side = 'left',
                                       title = 'Value',
                                       showgrid = F,
                                       overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                         legend = list(orientation = "h",
                                       y = -0.2,
                                       x = 0.32,
                                       title = ''),
                         xaxis = list(title = feature_to_plot),
                         title = Plottitle,
                         autosize = T,
                         margin = list(b = 50, l = 50, r=80)) %>%
          plotly::add_annotations(
            x= 0.5,
            y= 1.05,
            xref = "paper",
            yref = "paper",
            text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
            showarrow = F
          )
        l <- l + 1
      }
      p_return <- plotly::subplot(plotlist,
                                  nrows = numberoffacets,
                                  titleY = T,
                                  titleX = T,
                                  margin = 0.07,
                                  shareY = F,
                                  shareX = T
      )

    } else{
      # non faceted code -----------------------------------------------------------------------
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

      if (plot_type == 'residuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual_Values')
        ylabeltext <- 'Residual'
        Plottitle <- paste('Residuals for',feature_to_plot)
      } else if (plot_type == 'predictions'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type != 'Residual_Values')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
      } else if (plot_type == 'actuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual_Values')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual for',feature_to_plot)
      } else{
        print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
      }

      # plot factor as numeric is by default true
      Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(base::paste0(feature_to_plot,'_cat'))), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(base::paste0(feature_to_plot,'_cat')))))))

      # plot the data
      density_data <- dplyr::select(dplyr::bind_cols(list(data_set, predicted_dataset)), all_of(feature_to_plot))
      fit <- density_data %>%
        dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
        dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
        dplyr::pull() %>%
        stats::density()

      p_return <- Plot_data_to_plot %>%
        plotly::plot_ly(colors = c(first_colour, second_colour),
                        height = height,
                        width = width,
                        showlegend = T) %>%
        plotly::add_trace(x = ~get(base::paste0(feature_to_plot,'_cat')),
                          y = ~Average_value,
                          type="scatter",
                          mode="lines+markers",
                          color = ~Data_Type,
                          yaxis = "y2") %>%
        plotly::add_trace(x = fit$x,
                          y = fit$y,
                          type = "scatter",
                          mode = "lines",
                          fill = "tozeroy",
                          yaxis = "y",
                          name = "Density",
                          fillcolor = 'rgba(221,221,221,0.5)',
                          line  = list(color = 'rgba(221,221,221,0.7)')) %>%
        plotly::layout(yaxis = list(side = 'right',
                                    title = 'Density'),
                       yaxis2 = list(side = 'left',
                                     title = 'Target',
                                     showgrid = F,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.32,
                                     title = ''),
                       xaxis = list(title = feature_to_plot),
                       title = Plottitle,
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  }
  return(p_return)
}

