#' @title one_way_ave
#'
#' @description Creates a pretty html plot of one way actual vs expected by specified predictor.
#'
#' @param feature_to_plot A string of the variable to plot.
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats]{glm}, \link[stats]{lm}. If NULL, only actual values are plotted (no predictions or residuals).
#' @param target_variable String of target variable name in dataset.
#' @param data_set Data set to calculate the actual vs expected for. If no input default is to try and extract training data from model object.
#' @param plot_type one of "Residual", "predictions" or "actuals" defaults to "predictions"
#' @param plot_factor_as_numeric Set to TRUE to return \link[base]{data.frame} instead of creating \link[knitr]{kable}.
#' @param ordering Option to change the ordering of categories on the x axis, only for discrete categories. Default to the ordering of the factor. Other options are: 'alphabetical', 'Number of records', 'Average Value'
#' @param width Width of plot. When faceting horizontally this is a minimum: the plot is widened if needed so each facet keeps a readable width.
#' @param height Height of plot. When faceting vertically this is a minimum: the plot is made taller if needed so each facet keeps a readable height.
#' @param number_of_buckets Number of buckets for continuous variable plots
#' @param first_colour First colour to plot, usually the colour of actual.
#' @param second_colour Second colour to plot, usually the colour of predicted.
#' @param facetby Variable to facet the actual vs expect plots by.
#' @param facet_direction Direction to facet plots: "vertical" (default, stacked rows) or "horizontal" (side-by-side columns).
#' @param shared_axis When faceting, set to TRUE (default) so every facet uses the same scale on both y axes (target on the left, number of records / density on the right). FALSE lets each facet scale independently.
#' @param prediction_type Prediction type to be pasted to predict.glm if predict_function is NULL. Defaults to "response".
#' @param predict_function A custom prediction function can be provided here. It must return a \link[base]{data.frame} with an "Actual_Values" column, and a "Predicted_Values" column.
#' @param upper_percentile_to_cut For continuous variables this is what percentile to exclude from the upper end of the distribution. Defaults to 0.01, so the maximum percentile of the variable in the plot will be 0.99. Cutting off some of the distribution can help the views if outlier's are present in the data.
#' @param lower_percentile_to_cut For continuous variables this is what percentile to exclude from the lower end of the distribution. Defaults to 0.01, so the minimum percentile of the variable in the plot will be 0.01. Cutting off some of the distribution can help the views if outlier's are present in the data.
#' @param rounding_preference Number of decimal places to round numbers to in the hover tooltips. Defaults to 2.
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
#' meanage <- base::mean(titanic$Age, na.rm=TRUE)
#'
#' titanic  <- titanic  %>%
#'   dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
#'   dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
#'   dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
#'                 Age_25_50 = prettyglm::splineit(Age,25,50),
#'                 Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
#'   dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
#'                 Fare_250_600 = prettyglm::splineit(Fare,250,600))
#'
#' survival_model <- stats::glm(Survived ~
#'                                Sex:Age +
#'                                Fare +
#'                                Embarked +
#'                                SibSp +
#'                                Parch +
#'                                Cabintype,
#'                              data = titanic,
#'                              family = binomial(link = 'logit'))
#'
#' # Actuals Only (no model)
#' one_way_ave(feature_to_plot = 'Age',
#'             target_variable = 'Survived',
#'             data_set = titanic,
#'             number_of_buckets = 20)
#'
#' # Continuous Variable Example
#' one_way_ave(feature_to_plot = 'Age',
#'             model_object = survival_model,
#'             target_variable = 'Survived',
#'             data_set = titanic,
#'             number_of_buckets = 20,
#'             upper_percentile_to_cut = 0.1,
#'             lower_percentile_to_cut = 0.1)
#'
#' # Discrete Variable Example
#' one_way_ave(feature_to_plot = 'Pclass',
#'             model_object = survival_model,
#'             target_variable = 'Survived',
#'             data_set = titanic)
#'
#' # Custom Predict Function and facet
#' a_custom_predict_function <- function(target, model_object, dataset){
#'   dataset <- base::as.data.frame(dataset)
#'   Actual_Values <- dplyr::pull(dplyr::select(dataset, tidyselect::all_of(c(target))))
#'   if(class(Actual_Values) == 'factor'){
#'     Actual_Values <- base::as.numeric(as.character(Actual_Values))
#'   }
#'   Predicted_Values <- base::as.numeric(stats::predict(model_object, dataset, type='response'))
#'
#'   to_return <-  base::data.frame(Actual_Values = Actual_Values,
#'                                  Predicted_Values = Predicted_Values)
#'
#'   to_return <- to_return %>%
#'     dplyr::mutate(Predicted_Values = base::ifelse(Predicted_Values > 0.3,0.3,Predicted_Values))
#'   return(to_return)
#' }
#'
#' one_way_ave(feature_to_plot = 'Age',
#'             model_object = survival_model,
#'             target_variable = 'Survived',
#'             data_set = titanic,
#'             number_of_buckets = 20,
#'             upper_percentile_to_cut = 0.1,
#'             lower_percentile_to_cut = 0.1,
#'             predict_function = a_custom_predict_function,
#'             facetby = 'Pclass')
#'
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

one_way_ave <- function(
  feature_to_plot,
  data_set,
  target_variable,
  model_object = NULL,
  plot_type = 'predictions',
  plot_factor_as_numeric = FALSE,
  ordering = NULL,
  width = 800,
  height = 500,
  number_of_buckets = 30,
  first_colour = 'black',
  second_colour = '#cc4678',
  facetby = NULL,
  facet_direction = 'vertical',
  shared_axis = TRUE,
  prediction_type = 'response',
  predict_function = NULL,
  upper_percentile_to_cut = 0.01,
  lower_percentile_to_cut = 0,
  rounding_preference = 2
) {
  # Make sure plots can handle residuals as a plot_type input

  # Legend is centred under the plot area via xanchor; without it x positions
  # the legend's left edge, so it drifts off-centre as entry widths change.
  legend_layout <- list(
    orientation = "h",
    xanchor = 'center',
    x = 0.5,
    y = -0.2,
    title = list(text = '')
  )

  # d3 number format used in tooltips, e.g. ':.2f' for 2 decimal places
  num_fmt <- base::paste0(':.', rounding_preference, 'f')

  # Extract the actual and expected values -------------------------------------------
  # if provided data set is null then use the training data from model object
  if (is.null(data_set) == T) {
    # Extract training data from model object
    if (base::any(class(model_object) == 'workflow')) {
      # Workflow model objects here
      data_set <- tidy_workflow$fit$fit$fit$data
    } else if (base::any(class(model_object) == 'model_fit')) {
      # pasnip model objects here
      data_set <- model_object$fit$data
    } else {
      #stats::glm objects here
      data_set <- model_object$data
    }
  }

  # make predictions
  if (is.null(model_object)) {
    # No model provided: only actuals are available
    dataset_df <- base::as.data.frame(data_set)
    actual_vals <- dplyr::pull(dplyr::select(
      dataset_df,
      tidyselect::all_of(c(target_variable))
    ))
    if (methods::is(actual_vals, 'factor')) {
      actual_vals <- base::as.numeric(as.character(actual_vals))
    }
    predicted_dataset <- base::data.frame(
      Actual_Values = actual_vals,
      Predicted_Values = actual_vals
    )
    # Override plot_type since predictions/residuals are not available
    if (plot_type != 'actuals') {
      warning("No model_object provided. Defaulting plot_type to 'actuals'.")
      plot_type <- 'actuals'
    }
  } else if (is.null(predict_function) == T) {
    predicted_dataset <- prettyglm::predict_outcome(
      target = target_variable,
      model_object = model_object,
      dataset = data_set,
      prediction_type = prediction_type
    )
  } else {
    predicted_dataset <- predict_function(
      target = target_variable,
      model_object = model_object,
      dataset = data_set
    )
  }

  # tidy data for plotting different if factor or continuous -----------------------------------
  Plot_data <- dplyr::bind_cols(list(data_set, predicted_dataset))
  if (
    class(dplyr::select(Plot_data, tidyselect::matches(feature_to_plot))[[
      1
    ]]) %in%
      c('factor', 'integer', 'character')
  ) {
    # categorical logic
    if (
      base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0
    ) {
      Plot_data <- Plot_data[
        -base::which(base::is.na(Plot_data$Predicted_Values)),
      ]
    }

    if (is.null(facetby) == F) {
      # faceted code ---------------------------------------------------------------------------
      if (sum(is.na(Plot_data[[facetby]])) > 0) {
        base::warning(base::paste(
          base::sum(base::is.na(Plot_data[[facetby]])),
          'NA values detected in facet variable',
          facetby,
          'which are being removed. To plot this data, force NAs to be a categorical level'
        ))
      }
      numberoffacets <- base::length(na.omit(base::unique(dplyr::pull(dplyr::select(
        Plot_data,
        dplyr::all_of(facetby)
      )))))
      l <- 1
      plotlist <- list()
      facet_y_values <- c()
      facet_right_values <- c()
      while (l <= numberoffacets) {
        facettoplot <- na.omit(base::unique(dplyr::pull(dplyr::select(
          Plot_data,
          dplyr::all_of(facetby)
        ))))[l]

        # prep the data--------------------------------------------------------------
        Plot_data_inside <- Plot_data %>%
          dplyr::filter(get(facetby) == facettoplot) %>%
          dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
          dplyr::rename(Actual = Actual_Values) %>%
          dplyr::rename(Predicted = Predicted_Values) %>%
          dplyr::mutate(Residual = Actual - Predicted) %>%
          tidyr::pivot_longer(
            c(Actual, Predicted, Residual),
            names_to = 'Data_Type',
            values_to = 'value'
          ) %>%
          dplyr::group_by_at(c(feature_to_plot, facetby, 'Data_Type')) %>%
          dplyr::summarise(
            Average_value = mean(value),
            Number_of_Records = n(),
            .groups = 'drop'
          ) %>%
          dplyr::ungroup()

        if (plot_type == 'residuals') {
          Plot_data_to_plot <- dplyr::filter(
            Plot_data_inside,
            Data_Type == 'Residual'
          )
          ylabeltext <- 'Residual'
          Plottitle <- paste('Residuals for', feature_to_plot)
        } else if (plot_type == 'predictions') {
          Plot_data_to_plot <- dplyr::filter(
            Plot_data_inside,
            Data_Type != 'Residual'
          ) %>%
            dplyr::mutate(
              Number_of_Records = base::ifelse(
                Data_Type == 'Predicted',
                0,
                Number_of_Records
              )
            )
          ylabeltext <- target_variable
          Plottitle <- paste('Actual Vs Predicted for', feature_to_plot)
        } else if (plot_type == 'actuals') {
          Plot_data_to_plot <- dplyr::filter(
            Plot_data_inside,
            Data_Type == 'Actual'
          )
          ylabeltext <- target_variable
          Plottitle <- paste('Actuals for', feature_to_plot)
        } else {
          print(
            "plot_type must be one of: 'residuals', 'predictions' or 'actuals'"
          )
        }

        if (plot_factor_as_numeric == TRUE) {
          Plot_data_to_plot <- Plot_data_to_plot %>%
            dplyr::mutate_at(
              dplyr::vars(tidyselect::all_of(feature_to_plot)),
              ~ as.numeric(as.character(dplyr::pull(dplyr::select(
                Plot_data_to_plot,
                tidyselect::all_of(feature_to_plot)
              ))))
            )
        }

        facet_y_values <- c(facet_y_values, Plot_data_to_plot$Average_value)

        Count_data <- Plot_data_to_plot %>%
          dplyr::select(c(feature_to_plot, 'Number_of_Records')) %>%
          base::unique()
        facet_right_values <- c(facet_right_values, Count_data$Number_of_Records)

        plotlist[[l]] <- Plot_data_to_plot %>%
          plotly::plot_ly(
            colors = c(first_colour, second_colour),
            height = height,
            width = width,
            showlegend = base::ifelse(l == 1, T, F)
          ) %>%
          plotly::add_trace(
            x = ~ get(feature_to_plot),
            y = ~Average_value,
            type = "scatter",
            mode = "lines+markers",
            color = ~Data_Type,
            yaxis = "y2",
            hovertemplate = base::paste0(
              feature_to_plot, ': %{x}<br>',
              ylabeltext, ': %{y', num_fmt, '}'
            )
          ) %>%
          plotly::add_bars(
            data = Count_data,
            x = dplyr::pull(dplyr::select(
              Count_data,
              tidyselect::all_of(feature_to_plot)
            )),
            y = dplyr::pull(dplyr::select(
              Count_data,
              tidyselect::all_of('Number_of_Records')
            )),
            name = 'Number of records',
            hovertemplate = base::paste0(feature_to_plot, ': %{x}<br>Number of records: %{y:,}'),
            yaxis = 'y',
            marker = list(
              color = '#dddddd',
              line = list(width = 0, color = 'black')
            )
          ) %>%
          plotly::layout(
            yaxis = list(side = 'right', title = 'Number of Records'),
            yaxis2 = list(
              side = 'left',
              title = ylabeltext,
              showgrid = F,
              overlaying = base::paste0('y', as.character((l - 1) * 2 + 1))
            ),
            legend = legend_layout,
            xaxis = list(title = feature_to_plot),
            title = Plottitle,
            autosize = T,
            margin = list(b = 50, l = 50, r = 80)
          ) %>%
          # Sits on the top edge of its own panel, clear of the figure title.
          plotly::add_annotations(
            x = 0.5,
            y = 1,
            xref = "paper",
            yref = "paper",
            yanchor = 'bottom',
            text = base::paste0('<b>', facettoplot, '</b>'),
            showarrow = F
          )
        l <- l + 1
      }
      plotlist <- plotlist[!sapply(plotlist, is.null)]
      if (shared_axis) {
        plotlist <- apply_shared_y_range(
          plotlist,
          facet_y_values,
          facet_right_values
        )
      }
      p_return <- assemble_facet_subplot(
        plotlist = plotlist,
        facet_direction = facet_direction,
        width = width,
        height = height,
        left_title = ylabeltext,
        right_title = 'Number of Records',
        x_title = feature_to_plot,
        plot_title = Plottitle
      )
    } else {
      Plot_data <- Plot_data %>%
        dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
        dplyr::rename(Actual = Actual_Values) %>%
        dplyr::rename(Predicted = Predicted_Values) %>%
        dplyr::mutate(Residual = Actual - Predicted) %>%
        tidyr::pivot_longer(
          c(Actual, Predicted, Residual),
          names_to = 'Data_Type',
          values_to = 'value'
        ) %>%
        dplyr::group_by_at(c(feature_to_plot, 'Data_Type')) %>%
        dplyr::summarise(
          Average_value = mean(value),
          Number_of_Records = n(),
          .groups = 'drop'
        ) %>%
        dplyr::ungroup()

      # Create plots --------------------------------------------------------------------------------
      if (plot_type == 'residuals') {
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual')
        ylabeltext <- 'Residual'
        Plottitle <- paste('Residuals for', feature_to_plot)
      } else if (plot_type == 'predictions') {
        Plot_data_to_plot <- dplyr::filter(
          Plot_data,
          Data_Type != 'Residual'
        ) %>%
          dplyr::mutate(
            Number_of_Records = base::ifelse(
              Data_Type == 'Predicted',
              0,
              Number_of_Records
            )
          )
        ylabeltext <- target_variable
        Plottitle <- paste('Actual Vs Predicted for', feature_to_plot)
      } else if (plot_type == 'actuals') {
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual')
        ylabeltext <- target_variable
        Plottitle <- paste(
          'One Way Plot of ',
          target_variable,
          ' by ',
          feature_to_plot
        )
      } else {
        print(
          "plot_type must be one of: 'residuals', 'predictions' or 'actuals'"
        )
      }

      if (plot_factor_as_numeric == TRUE) {
        Plot_data_to_plot <- Plot_data_to_plot %>%
          dplyr::mutate_at(
            dplyr::vars(tidyselect::all_of(feature_to_plot)),
            ~ as.numeric(as.character(dplyr::pull(dplyr::select(
              Plot_data_to_plot,
              tidyselect::all_of(feature_to_plot)
            ))))
          )
      }

      # Change ordering if specified -----------------------------------------------------------------
      if (base::is.null(ordering) == F) {
        order_option <- ordering
        if (length(order_option) > 1) {
          Plot_data_to_plot <- Plot_data_to_plot %>%
            dplyr::mutate_at(
              .vars = c(feature_to_plot),
              .funs = ~ base::factor(., levels = order_option)
            )
        } else {
          if (ordering == 'alphabetical') {
            order_option <- feature_to_plot
          } else if (ordering == 'Number of records') {
            order_option <- 'Number_of_Records'
          } else if (ordering == 'Average Value') {
            order_option <- 'Average_value'
          } else {
            base::print(
              'You have entered an incorrect ordering option. Please enter: alphabetical, Number of records, Average Value or a vector of level names'
            )
          }
          Plot_data_to_plot <- Plot_data_to_plot %>%
            dplyr::arrange(get(order_option)) %>%
            dplyr::mutate_at(
              .vars = c(feature_to_plot),
              .funs = ~ base::factor(., base::unique(.))
            )
        }
      }

      # Create plot
      p_return <- Plot_data_to_plot %>%
        plotly::plot_ly(
          colors = c(first_colour, second_colour),
          height = height,
          width = width,
          showlegend = T
        ) %>%
        plotly::add_trace(
          x = ~ get(feature_to_plot),
          y = ~Average_value,
          type = "scatter",
          mode = "lines+markers",
          color = ~Data_Type,
          yaxis = "y2",
          hovertemplate = base::paste0(
            feature_to_plot, ': %{x}<br>',
            ylabeltext, ': %{y', num_fmt, '}'
          )
        ) %>%
        plotly::add_bars(
          x = ~ get(feature_to_plot),
          y = ~Number_of_Records,
          name = 'Number of records',
          hovertemplate = base::paste0(feature_to_plot, ': %{x}<br>Number of records: %{y:,}'),
          yaxis = 'y',
          marker = list(
            color = '#dddddd',
            line = list(width = 0, color = 'black')
          )
        ) %>%
        plotly::layout(
          yaxis = list(side = 'right', title = 'Number of Records'),
          yaxis2 = list(
            side = 'left',
            title = ylabeltext,
            showgrid = F,
            overlaying = 'y'
          ),
          legend = legend_layout,
          xaxis = list(title = feature_to_plot),
          title = Plottitle,
          autosize = T,
          margin = list(b = 50, l = 50, r = 80)
        )
      return(p_return)
    }
  } else {
    # continuous logic  -------------------------------------------------------------------------
    if (
      base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0
    ) {
      Plot_data <- Plot_data[
        -base::which(base::is.na(Plot_data$Predicted_Values)),
      ]
    }
    # default number of buckets for continuous variables is 30
    if (base::is.null(number_of_buckets) == T) {
      number_of_buckets <- 30
    }
    # prep the data and cut the data
    Plot_data <- Plot_data %>%
      dplyr::filter(
        base::get(feature_to_plot) <=
          as.numeric(stats::quantile(
            dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot)),
            probs = c(1 - upper_percentile_to_cut),
            na.rm = T
          ))
      ) %>%
      dplyr::filter(
        base::get(feature_to_plot) >=
          as.numeric(stats::quantile(
            dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot)),
            probs = c(lower_percentile_to_cut),
            na.rm = T
          ))
      )

    Plot_data[, base::paste0(feature_to_plot, '_cat')] = prettyglm::cut3(
      x = dplyr::pull(dplyr::select(
        Plot_data,
        tidyselect::all_of(feature_to_plot)
      )),
      g = number_of_buckets
    )
    if (is.null(facetby) == F) {
      # faceted code ---------------------------------------------------------------------------
      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(
        Plot_data,
        dplyr::all_of(facetby)
      ))))
      l <- 1
      # Facets can be skipped (see the density check below), so panels are
      # indexed separately: the overlaying axis names below depend on a panel's
      # position in the final list, not on which facet it came from.
      plot_index <- 1
      plotlist <- list()
      facet_y_values <- c()
      facet_right_values <- c()
      while (l <= numberoffacets) {
        facettoplot <- base::unique(dplyr::pull(dplyr::select(
          Plot_data,
          dplyr::all_of(facetby)
        )))[l]

        # prep the data
        Plot_data_inside <- Plot_data %>%
          dplyr::filter(get(facetby) == facettoplot) %>%
          dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
          dplyr::rename(Actual = Actual_Values) %>%
          dplyr::rename(Predicted = Predicted_Values) %>%
          dplyr::mutate(Residual = Actual - Predicted) %>%
          tidyr::pivot_longer(
            c(Actual, Predicted, Residual),
            names_to = 'Data_Type',
            values_to = 'value'
          ) %>%
          dplyr::group_by_at(c(
            base::paste0(feature_to_plot, '_cat'),
            'Data_Type'
          )) %>%
          dplyr::summarise(
            Average_value = mean(value),
            Number_of_Records = n(),
            .groups = 'drop'
          ) %>%
          dplyr::ungroup()

        if (plot_type == 'residuals') {
          Plot_data_to_plot <- dplyr::filter(
            Plot_data_inside,
            Data_Type == 'Residual'
          )
          ylabeltext <- 'Residual'
          Plottitle <- paste('Residuals for', feature_to_plot)
        } else if (plot_type == 'predictions') {
          Plot_data_to_plot <- dplyr::filter(
            Plot_data_inside,
            Data_Type != 'Residual'
          ) %>%
            dplyr::mutate(
              Number_of_Records = base::ifelse(
                Data_Type == 'Predicted',
                0,
                Number_of_Records
              )
            )
          ylabeltext <- target_variable
          Plottitle <- paste('Actual Vs Predicted for', feature_to_plot)
        } else if (plot_type == 'actuals') {
          Plot_data_to_plot <- dplyr::filter(
            Plot_data_inside,
            Data_Type == 'Actual'
          )
          ylabeltext <- target_variable
          Plottitle <- paste('Actual for', feature_to_plot)
        } else {
          print(
            "plot_type must be one of: 'residuals', 'predictions' or 'actuals'"
          )
        }

        # plot factor as numeric is by default true
        Plot_data_to_plot <- Plot_data_to_plot %>%
          dplyr::mutate_at(
            dplyr::vars(tidyselect::all_of(base::paste0(
              feature_to_plot,
              '_cat'
            ))),
            ~ as.numeric(as.character(dplyr::pull(dplyr::select(
              Plot_data_to_plot,
              tidyselect::all_of(base::paste0(feature_to_plot, '_cat'))
            ))))
          )

        # Try the density plot
        density_data <- dplyr::bind_cols(list(data_set, predicted_dataset)) %>%
          dplyr::filter(get(facetby) == facettoplot)
        fit <- tryCatch(
          {
            density_data %>%
              dplyr::filter(
                base::get(feature_to_plot) <=
                  as.numeric(stats::quantile(
                    dplyr::select(
                      density_data,
                      tidyselect::all_of(feature_to_plot)
                    ),
                    probs = c(1 - upper_percentile_to_cut),
                    na.rm = T
                  ))
              ) %>%
              dplyr::filter(
                base::get(feature_to_plot) >=
                  as.numeric(stats::quantile(
                    dplyr::select(
                      density_data,
                      tidyselect::all_of(feature_to_plot)
                    ),
                    probs = c(lower_percentile_to_cut),
                    na.rm = T
                  ))
              ) %>%
              dplyr::select(tidyselect::all_of(feature_to_plot)) %>%
              dplyr::pull() %>%
              stats::density(., na.rm = T)
          },
          error = function(e) NULL
        )

        if (is.null(fit)) {
          warning(paste(
            "Insufficient data points for density in",
            facettoplot,
            "- skipping this facet"
          ))
          l <- l + 1
          next
        }
        facet_y_values <- c(facet_y_values, Plot_data_to_plot$Average_value)
        facet_right_values <- c(facet_right_values, fit$y)

        plotlist[[plot_index]] <- Plot_data_to_plot %>%
          plotly::plot_ly(
            colors = c(first_colour, second_colour),
            height = height,
            width = width,
            showlegend = base::ifelse(plot_index == 1, T, F)
          ) %>%
          plotly::add_trace(
            x = ~ get(base::paste0(feature_to_plot, '_cat')),
            y = ~Average_value,
            type = "scatter",
            mode = "lines+markers",
            color = ~Data_Type,
            yaxis = "y2",
            hovertemplate = base::paste0(
              feature_to_plot, ': %{x', num_fmt, '}<br>',
              ylabeltext, ': %{y', num_fmt, '}'
            )
          ) %>%
          plotly::add_trace(
            x = fit$x,
            y = fit$y,
            type = "scatter",
            mode = "lines",
            fill = "tozeroy",
            yaxis = "y",
            name = "Density",
            hovertemplate = base::paste0(
              feature_to_plot, ': %{x', num_fmt, '}<br>Density: %{y:.3~g}'
            ),
            fillcolor = 'rgba(221,221,221,0.5)',
            line = list(color = 'rgba(221,221,221,0.7)')
          ) %>%
          plotly::layout(
            yaxis = list(side = 'right', title = 'Density'),
            yaxis2 = list(
              side = 'left',
              title = ylabeltext,
              showgrid = F,
              overlaying = base::paste0(
                'y',
                as.character((plot_index - 1) * 2 + 1)
              )
            ),
            legend = legend_layout,
            xaxis = list(title = feature_to_plot),
            title = Plottitle,
            autosize = T,
            margin = list(b = 50, l = 50, r = 80)
          ) %>%
          # Sits on the top edge of its own panel, clear of the figure title.
          plotly::add_annotations(
            x = 0.5,
            y = 1,
            xref = "paper",
            yref = "paper",
            yanchor = 'bottom',
            text = base::paste0('<b>', facettoplot, '</b>'),
            showarrow = F
          )
        plot_index <- plot_index + 1
        l <- l + 1
      }
      plotlist <- plotlist[!sapply(plotlist, is.null)]
      if (shared_axis) {
        plotlist <- apply_shared_y_range(
          plotlist,
          facet_y_values,
          facet_right_values
        )
      }
      p_return <- assemble_facet_subplot(
        plotlist = plotlist,
        facet_direction = facet_direction,
        width = width,
        height = height,
        left_title = ylabeltext,
        right_title = 'Density',
        x_title = feature_to_plot,
        plot_title = Plottitle
      )
    } else {
      # non faceted code -----------------------------------------------------------------------
      Plot_data <- Plot_data %>%
        dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
        dplyr::rename(Actual = Actual_Values) %>%
        dplyr::rename(Predicted = Predicted_Values) %>%
        dplyr::mutate(Residual = Actual - Predicted) %>%
        tidyr::pivot_longer(
          c(Actual, Predicted, Residual),
          names_to = 'Data_Type',
          values_to = 'value'
        ) %>%
        dplyr::group_by_at(c(
          base::paste0(feature_to_plot, '_cat'),
          'Data_Type'
        )) %>%
        dplyr::summarise(
          Average_value = mean(value),
          Number_of_Records = n(),
          .groups = 'drop'
        ) %>%
        dplyr::ungroup()

      if (plot_type == 'residuals') {
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual')
        ylabeltext <- 'Residual'
        Plottitle <- paste('Residuals for', feature_to_plot)
      } else if (plot_type == 'predictions') {
        Plot_data_to_plot <- dplyr::filter(
          Plot_data,
          Data_Type != 'Residual'
        ) %>%
          dplyr::mutate(
            Number_of_Records = base::ifelse(
              Data_Type == 'Predicted',
              0,
              Number_of_Records
            )
          )
        ylabeltext <- target_variable
        Plottitle <- paste('Actual Vs Predicted for', feature_to_plot)
      } else if (plot_type == 'actuals') {
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual for', feature_to_plot)
      } else {
        print(
          "plot_type must be one of: 'residuals', 'predictions' or 'actuals'"
        )
      }

      # plot factor as numeric is by default true
      Plot_data_to_plot <- Plot_data_to_plot %>%
        dplyr::mutate_at(
          dplyr::vars(tidyselect::all_of(base::paste0(
            feature_to_plot,
            '_cat'
          ))),
          ~ as.numeric(as.character(dplyr::pull(dplyr::select(
            Plot_data_to_plot,
            tidyselect::all_of(base::paste0(feature_to_plot, '_cat'))
          ))))
        )

      # plot the data
      density_data <- dplyr::select(
        dplyr::bind_cols(list(data_set, predicted_dataset)),
        all_of(feature_to_plot)
      )
      fit <- density_data %>%
        dplyr::filter(
          base::get(feature_to_plot) <=
            as.numeric(stats::quantile(
              dplyr::select(density_data, tidyselect::all_of(feature_to_plot)),
              probs = c(1 - upper_percentile_to_cut),
              na.rm = T
            ))
        ) %>%
        dplyr::filter(
          base::get(feature_to_plot) >=
            as.numeric(stats::quantile(
              dplyr::select(density_data, tidyselect::all_of(feature_to_plot)),
              probs = c(lower_percentile_to_cut),
              na.rm = T
            ))
        ) %>%
        dplyr::pull() %>%
        stats::density()

      p_return <- Plot_data_to_plot %>%
        plotly::plot_ly(
          colors = c(first_colour, second_colour),
          height = height,
          width = width,
          showlegend = T
        ) %>%
        plotly::add_trace(
          x = ~ get(base::paste0(feature_to_plot, '_cat')),
          y = ~Average_value,
          type = "scatter",
          mode = "lines+markers",
          color = ~Data_Type,
          yaxis = "y2",
          hovertemplate = base::paste0(
            feature_to_plot, ': %{x', num_fmt, '}<br>',
            ylabeltext, ': %{y', num_fmt, '}'
          )
        ) %>%
        plotly::add_trace(
          x = fit$x,
          y = fit$y,
          type = "scatter",
          mode = "lines",
          fill = "tozeroy",
          yaxis = "y",
          name = "Density",
          hovertemplate = base::paste0(
            feature_to_plot, ': %{x', num_fmt, '}<br>Density: %{y:.3~g}'
          ),
          fillcolor = 'rgba(221,221,221,0.5)',
          line = list(color = 'rgba(221,221,221,0.7)')
        ) %>%
        plotly::layout(
          yaxis = list(side = 'right', title = 'Density'),
          yaxis2 = list(
            side = 'left',
            title = ylabeltext,
            showgrid = F,
            overlaying = 'y'
          ),
          legend = legend_layout,
          xaxis = list(title = feature_to_plot),
          title = Plottitle,
          autosize = T,
          margin = list(b = 50, l = 50, r = 80)
        )
      return(p_return)
    }
  }
  return(p_return)
}

#' Give every facet panel the same y axis ranges
#'
#' @param plotlist List of plotly panels, one per facet.
#' @param y_values All values plotted on the left axis (`yaxis2`), across every facet.
#' @param right_values All values plotted on the right axis (`yaxis`: counts or
#'   density), across every facet.
#'
#' @return plotlist with common `yaxis` and `yaxis2` ranges set on each panel.
#' @noRd
apply_shared_y_range <- function(plotlist, y_values, right_values) {
  layout_args <- list()

  y_values <- y_values[base::is.finite(y_values)]
  if (base::length(y_values) > 0) {
    y_lim <- base::range(y_values)
    # Pad so the extreme markers aren't clipped at the panel edge.
    pad <- base::diff(y_lim) * 0.05
    if (pad == 0) {
      pad <- base::max(base::abs(y_lim[1]) * 0.05, 0.01)
    }
    layout_args$yaxis2 <- list(range = y_lim + c(-pad, pad))
  }

  # Counts and density are never negative, so anchor the right axis at zero.
  right_values <- right_values[base::is.finite(right_values)]
  if (base::length(right_values) > 0 && base::max(right_values) > 0) {
    layout_args$yaxis <- list(range = c(0, base::max(right_values) * 1.05))
  }

  if (base::length(layout_args) == 0) {
    return(plotlist)
  }
  base::lapply(plotlist, function(p) {
    base::do.call(plotly::layout, c(list(p), layout_args))
  })
}

#' Assemble faceted one-way plots into a single subplot
#'
#' Each facet panel is a dual-axis figure (bars/density on `yaxis`, averages on
#' an overlaid `yaxis2`), so the panels must not share axes. This helper handles
#' the layout concerns that are easy to get wrong as the number of facets grows:
#' inter-panel spacing, equal panel sizes, a single set of axis titles rather
#' than one per panel, and a figure large enough that panels stay legible.
#'
#' @param plotlist List of plotly panels, one per facet.
#' @param facet_direction "vertical" or "horizontal".
#' @param width,height Requested figure size; grown if needed to keep panels readable.
#' @param left_title,right_title,x_title Axis titles, drawn once for the whole figure.
#' @param plot_title Figure title.
#'
#' @return A single plotly object.
#' @noRd
assemble_facet_subplot <- function(
  plotlist,
  facet_direction,
  width,
  height,
  left_title,
  right_title,
  x_title,
  plot_title
) {
  n <- base::length(plotlist)
  if (n == 0) {
    base::stop(
      "No facets could be plotted. Check the facet variable has levels with enough data."
    )
  }

  # Spacing between panels, as a fraction of the figure. plotly applies this to
  # each side of a panel, so a fixed value larger than half a panel's slot makes
  # panels collapse once there are more than a handful of facets.
  panel_margin <- base::min(0.07, 0.2 / n)

  # plotly applies the margin between panels but not at the outer edges, which
  # leaves interior panels smaller than the two end ones. Pad the interior slots
  # so every panel is drawn the same size.
  drawn <- (1 - 2 * panel_margin * (n - 1)) / n
  if (n == 1) {
    sizes <- 1
  } else {
    sizes <- c(
      drawn + panel_margin,
      base::rep(drawn + 2 * panel_margin, base::max(n - 2, 0)),
      drawn + panel_margin
    )
  }

  # Grow the figure so panels keep a usable size as facets are added, rather
  # than squeezing more panels into the requested width/height.
  if (facet_direction == 'horizontal') {
    total_width <- base::max(width, 260 * n)
    total_height <- height
    p_return <- plotly::subplot(
      plotlist,
      nrows = 1,
      titleY = FALSE,
      titleX = FALSE,
      margin = panel_margin,
      shareY = FALSE,
      shareX = FALSE,
      widths = sizes
    )
  } else {
    total_width <- width
    total_height <- base::max(height, 200 * n)
    p_return <- plotly::subplot(
      plotlist,
      nrows = n,
      titleY = FALSE,
      titleX = FALSE,
      margin = panel_margin,
      shareY = FALSE,
      shareX = TRUE,
      heights = sizes
    )
  }

  # Offsets are given in pixels (xshift/yshift, and a legend positioned against
  # the whole figure) rather than as paper fractions, so they hold even when
  # width/height are NULL and the final figure size isn't known here.
  plot_margins <- list(t = 90, b = 100, l = 90, r = 90)

  p_return <- p_return %>%
    plotly::layout(
      title = plot_title,
      autosize = FALSE,
      margin = plot_margins,
      # Pinned to the bottom of the figure, inside the bottom margin and below
      # the x axis title.
      legend = list(
        orientation = "h",
        xanchor = 'center',
        x = 0.5,
        yref = 'container',
        yanchor = 'bottom',
        y = 0,
        title = ''
      )
    ) %>%
    # One set of axis titles for the whole figure, instead of one per panel.
    plotly::add_annotations(
      x = 0,
      y = 0.5,
      xshift = -70,
      text = left_title,
      textangle = -90,
      xref = "paper",
      yref = "paper",
      xanchor = 'center',
      yanchor = 'middle',
      showarrow = FALSE
    ) %>%
    plotly::add_annotations(
      x = 1,
      y = 0.5,
      xshift = 70,
      text = right_title,
      textangle = 90,
      xref = "paper",
      yref = "paper",
      xanchor = 'center',
      yanchor = 'middle',
      showarrow = FALSE
    ) %>%
    plotly::add_annotations(
      x = 0.5,
      y = 0,
      yshift = -45,
      text = x_title,
      xref = "paper",
      yref = "paper",
      xanchor = 'center',
      yanchor = 'top',
      showarrow = FALSE
    )

  # Set the size directly rather than through layout(), which deprecated
  # width/height. Both slots are needed: the widget fields size the container,
  # and the layout fields would otherwise keep the per-panel size the individual
  # panels carried into the subplot, undoing the growth applied above.
  p_return$x$layout$width <- total_width
  p_return$x$layout$height <- total_height
  p_return$width <- total_width
  p_return$height <- total_height

  p_return
}
