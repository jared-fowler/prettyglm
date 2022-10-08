#' @title pretty_relativities
#'
#' @description Creates a pretty html plot of model relativities including base Levels.
#'
#' @param feature_to_plot A string of the variable to plot.
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats]{glm}, \link[stats]{lm},  \link[parsnip]{linear_reg} or \link[parsnip]{logistic_reg}
#' @param plot_approx_ci Set to TRUE to include confidence intervals in summary table. Warning, can be computationally expensive.
#' @param relativity_transform String of the function to be applied to the model estimate to calculate the relativity, for example: 'exp(estimate)'. Default is for relativity to be 'exp(estimate)-1'.
#' @param relativity_label String of label to give to relativity column if you want to change the title to your use case, some users may prefer to refer to this as odds ratio.
#' @param ordering Option to change the ordering of categories on the x axis, only for discrete categories. Default to the ordering of the fitted factor. Other options are: 'alphabetical', 'Number of records', 'Average Value'
#' @param plot_factor_as_numeric Set to TRUE to return \link[base]{data.frame} instead of creating \link[knitr]{kable}.
#' @param width Width of plot
#' @param height Height of plot
#' @param return_data Set to TRUE to return data set instead of plot
#' @param iteractionplottype If plotting the relativity for an interaction variable you can "facet" or "colour" by one of the interaction variables. Defaults to null.
#' @param facetorcolourtby If iteractionplottype is not Null, then this is the variable in the interaction you want to colour or facet by.
#' @param percentile_to_cut For continuous variables what percentile to cut off each end of the distribution. Defaults to 0.01. Cutting off some of the distribution can help the views if outliers are present in the training data.
#'
#' @return plotly plot of fitted relativities. \link[base]{data.frame} if return_data = TRUE.
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
#' survival_model <- stats::glm(Survived ~
#'                               Pclass +
#'                               Sex +
#'                               Age +
#'                               Fare +
#'                               Embarked +
#'                               SibSp +
#'                               Parch +
#'                               Cabintype,
#'                              data = titanic,
#'                              family = binomial(link = 'logit'))
#' pretty_relativities(feature_to_plot = 'Pclass',
#'                     model_object = survival_model)
#' @export
#' @importFrom tibble "tibble"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @import dplyr
#' @import plotly
#'

pretty_relativities <- function(feature_to_plot, model_object, plot_approx_ci = TRUE, relativity_transform = 'exp(estimate)-1', relativity_label = 'Relativity', ordering = NULL, plot_factor_as_numeric = FALSE, width = 800, height = 500, return_data = FALSE, iteractionplottype = NULL, facetorcolourtby = NULL, percentile_to_cut = 0.01){
  # fix colouring to add trace markers and lines
  # interacted cts varas, all combos of interaction variable
  # all types in interacted variable with splines
  # Check maths behind just chucking the relativity in there multiplicative for cts variables


  # Fix for global variables
  tidy_workflow <- NULL
  Variable <- NULL
  Relativity <- NULL
  relativity <- NULL
  Std.error <- NULL
  Approx_Upper_95CI <- NULL
  Approx_Lower_95CI <- NULL
  name <- NULL
  number_of_records <- NULL

  # Create relativity function from input
  base::eval(base::parse(text = base::paste('relativity <- function(estimate) { return(' , relativity_transform , ')}', sep='')))

  # Tidy model coefficients
  complete_factor_summary_df <- prettyglm::pretty_coefficients(model_object = model_object, relativity_transform = relativity_transform, return_data = T)

  # Extract training data from model object
  if (base::any(class(model_object) == 'workflow')){
    # workflow model objects here
    training_data <- tidy_workflow$fit$fit$fit$data
  } else if(base::any(class(model_object) == 'model_fit')){
    # pasnip model objects here
    training_data <- model_object$fit$data
  } else{
    #stats::glm objects here
    training_data <- model_object$data
  }

  # Add count of number of records for categorical variables
  suppressWarnings({count_df_all <- tibble::tibble()
  for (factor_name in base::unique(dplyr::pull(dplyr::select(complete_factor_summary_df, Variable)))){
    # For normal columns add on the number of records
    if ((factor_name != "(Intercept)") & ((base::grepl(":", factor_name)) == F)){
      count_df <- dplyr::select(training_data, tidyselect::all_of(factor_name)) %>%
        dplyr::group_by_at(tidyselect::all_of(factor_name)) %>%
        dplyr::summarise(number_of_records = dplyr::n(), .groups = 'drop') %>%
        dplyr::ungroup()
      count_df <- count_df %>% dplyr::mutate(Variable = base::rep(factor_name, base::nrow(count_df))) %>%
        dplyr::rename(Level = factor_name)
      count_df_all <- base::rbind(count_df_all,count_df)
    }
    # For interaction terms
    if (base::grepl(":", factor_name) == T){
      ivariable1 <- base::unlist(base::strsplit(factor_name, ':'))[1]
      ivariable2 <- base::unlist(base::strsplit(factor_name, ':'))[2]
      # For categorical categorical
      if ((class(dplyr::pull(dplyr::select(training_data, ivariable1))) %in% c('factor', 'character')) & ((class(dplyr::pull(dplyr::select(training_data, ivariable2))) %in% c('factor', 'character')))){
        count_df <- dplyr::select(training_data, c(ivariable1,ivariable2)) %>%
          dplyr::group_by_at(c(ivariable1,ivariable2)) %>%
          dplyr::summarise(number_of_records = dplyr::n(), .groups = 'drop') %>%
          dplyr::ungroup()
        count_df <- count_df %>%
          dplyr::mutate(Variable = factor_name) %>%
          dplyr::mutate(Level = base::paste0(base::get(ivariable1),':',base::get(ivariable2))) %>%
          dplyr::select(.,c('Level', 'number_of_records', 'Variable'))
        count_df_all <- base::rbind(count_df_all,count_df)
      }
      # For continuous continuous

      # For continuous categorical

      # handle 3 or more interactions
    }
  }
  complete_factor_summary_df <- dplyr::left_join(complete_factor_summary_df, count_df_all, by = c('Level' = 'Level', 'Variable' = 'Variable'))
  })

  # Discrete relativities ------------------------------------------------------------
  if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factormain") {
    # Filter to the variable we are plotting
    plot_data <- complete_factor_summary_df %>%
      dplyr::filter(Variable == feature_to_plot)

    # Get the number of records in each category
    factor_name <- base::unique(dplyr::pull(dplyr::select(plot_data, Variable)))
    count_df <- dplyr::select(training_data, tidyselect::all_of(factor_name)) %>%
      dplyr::group_by_at(tidyselect::all_of(factor_name)) %>%
      dplyr::summarise(number_of_records = dplyr::n(), .groups = 'drop') %>%
      dplyr::ungroup()
    count_df <- count_df %>% dplyr::mutate(Variable = base::rep(factor_name, base::nrow(count_df))) %>%
      dplyr::rename(Level = factor_name)
    plot_data <- dplyr::left_join(plot_data, count_df, by = c('Level' = 'Level', 'Variable' = 'Variable'))

    # Change the variable to numeric for plotting if needed
    if (plot_factor_as_numeric == TRUE){
      plot_data <- plot_data %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of('Level')), ~ as.numeric(as.character(dplyr::pull(dplyr::select(plot_data, tidyselect::all_of('Level'))))))
    }

    # Change ordering if specified
    if (base::is.null(ordering) ==  FALSE){
      if (base::length(ordering) >1){
        order_option <- ordering
        plot_data <- plot_data %>%
          dplyr::mutate_at(.vars = c('Level'), .funs = ~factor(. ,Levels = order_option))
      } else{
        if (ordering == 'alphabetical'){
          order_option <-  'Level'
        } else if (ordering == 'relativity'){
          order_option <- 'Relativity'
        } else if (ordering == 'pvalue'){
          order_option <- 'P.Value'
        } else{
          base::warning('You have entered an incorrect ordering option. Please enter: alphabetical, relativity, pvalue or a vector of level names')
        }
        plot_data <- plot_data %>%
          dplyr::arrange(get(order_option)) %>%
          dplyr::mutate_at(.vars = c('Level'), .funs = ~base::factor(., base::unique(.)))
      }
    } else{
      if (plot_factor_as_numeric == FALSE & (base::is.null(ordering) ==  FALSE) == FALSE){
        # if not other ordering, and no interaction, then order by the factor levels of the training dataset
        if (base::grepl(":", factor_name) == F){
          plot_data <- plot_data %>% dplyr::mutate_at(.vars = c("Level"),
                                                      .funs = ~factor(., levels = base::levels(dplyr::pull(dplyr::select(training_data, tidyselect::all_of(feature_to_plot))))))
        }
      }
    }

    # add confidence interval of 2* the standard error
    plot_data <- plot_data %>%
      dplyr::mutate(Approx_Upper_95CI = (Relativity + 2*relativity(Std.error)),
                    Approx_Lower_95CI = (Relativity - 2*relativity(Std.error)))  %>%
      tidyr::pivot_longer(cols = c(Relativity, Approx_Upper_95CI, Approx_Lower_95CI))

    if (plot_approx_ci == FALSE){
      plot_data <- plot_data %>% dplyr::filter(name == 'relativity')
    }

    # Create plot
    p_return <- plot_data %>%
        dplyr::mutate(number_of_records = base::ifelse(name == 'Relativity', number_of_records, 0)) %>%
        plotly::plot_ly(colors = if(plot_approx_ci == TRUE) c('grey', 'grey', 'black') else c('black'),
                        linetypes = if(plot_approx_ci == TRUE) c('dash', 'dash', 'solid') else c('solid'),
                        height = height,
                        width = width) %>%
        plotly::add_markers(x = ~Level,
                            y = ~value,
                            color = ~name,
                            type = "scatter",
                            showlegend = FALSE) %>%
        plotly::add_lines(x = ~Level,
                          y = ~value,
                          linetype = ~name,
                          color = ~name) %>%
        plotly::add_bars(
          x = ~Level,
          y = ~number_of_records,
          yaxis = 'y2',
          marker = list(color = '#dddddd',
                        line = list(width=0,
                                    color='black')),
          showlegend = FALSE
        ) %>%
        plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot),
                       yaxis2 = list(side = 'right',
                                     title = 'Number of Records',
                                     showgrid = FALSE),
                       yaxis = list(overlaying='y2',
                                    side = 'left',
                                    title = relativity_label, #relativity_label
                                    showgrid = TRUE),
                       legend = list(orientation = "h",
                                     xanchor = "center",
                                     x=0.5,
                                     y=-0.2),
                       autosize = TRUE,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)

    # Put this in an interaction plot if statemtent
    if (iteractionplottype == 'facet'){
      # add columns of the variable names
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[1]] <- base::unlist(base::lapply(base::strsplit(plot_data$Level, ':'), `[[`, 1))
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[2]] <- base::unlist(base::lapply(base::strsplit(plot_data$Level, ':'), `[[`, 2))

      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(plot_data, dplyr::all_of(facetorcolourtby)))))

      l <- 1
      plotlist <- list()
      while (l <= numberoffacets){
        facettoplot <- base::unique(dplyr::pull(dplyr::select(plot_data, dplyr::all_of(facetorcolourtby))))[l]
        xaxisvariable <- stringr::str_remove(stringr::str_remove(factor_name, facetorcolourtby),":")

        plotlist[[l]] <- plot_data %>%
          dplyr::filter(get(facetorcolourtby) == facettoplot) %>%
          dplyr::mutate(number_of_records = base::ifelse(name == 'Relativity', number_of_records, 0)) %>%
          plotly::plot_ly(colors = if(plot_approx_ci == TRUE) c('grey', 'grey', 'black') else c('black'),
                          linetypes = if(plot_approx_ci == TRUE) c('dash', 'dash', 'solid') else c('solid'),
                          height = height,
                          width = width,
                          showlegend = base::ifelse(l==1,T,F)) %>%
          plotly::add_trace(x = ~get(xaxisvariable),
                            y = ~value,
                            type="scatter",
                            mode="lines+markers",
                            color = ~name,
                            linetype = ~name,
                            yaxis = "y2") %>%
          plotly::add_bars(
            x = ~get(xaxisvariable),
            y = ~number_of_records,
            yaxis = 'y',
            marker = list(color = '#dddddd',
                          line = list(width=0,
                                      color='black')),
            showlegend = FALSE
          ) %>%
          plotly::layout(title = base::paste(facettoplot),
                         yaxis = list(#overlaying='y2',
                                      side = 'right',
                                      title = 'Number of Records', #relativity_label
                                      showgrid = TRUE
                                      ),
                         yaxis2 = list(side = 'left',
                                       title = relativity_label,
                                       showgrid = FALSE,
                                       overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                         xaxis = list(title = xaxisvariable),
                         legend = list(orientation = "h",
                                       xanchor = "center",
                                       x=0.5,
                                       y=-0.2),
                         autosize = TRUE,
                         margin = list(b = 50, l = 50, r=80)) %>%
          plotly::add_annotations(
            x= 0.5,
            y= 1,
            xref = "paper",
            yref = "paper",
            text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
            showarrow = F
          )
        l <- l + 1

      }
      p_return <-
        plotly::subplot(plotlist,
                        nrows = numberoffacets,
                        titleY = T,
                        titleX = T,
                        margin = 0.07,
                        shareY = F,
                        shareX = T) %>%
        plotly::layout(title = base::paste(relativity_label, 'for', factor_name, 'interaction', 'faceted by', facetorcolourtby))
      return(p_return)
    } else if (iteractionplottype == 'colour'){
      # add columns of the variable names
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[1]] <- base::unlist(base::lapply(base::strsplit(plot_data$Level, ':'), `[[`, 1))
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[2]] <- base::unlist(base::lapply(base::strsplit(plot_data$Level, ':'), `[[`, 2))

      # aggregate the number of records
      xaxisvariable <- stringr::str_remove(stringr::str_remove(factor_name, facetorcolourtby),":")
      agg_number_to_plot <- plot_data %>%
        dplyr::group_by_at(xaxisvariable) %>%
        dplyr::summarise(number_of_records = sum(number_of_records), .groups = 'drop')

      plot_datafacet <- plot_data %>%
        dplyr::filter(name == 'Relativity') %>%
        dplyr::select(-('number_of_records')) %>%
        dplyr::left_join(agg_number_to_plot, by = xaxisvariable)

      p_return <- plot_datafacet %>%
        dplyr::mutate(number_of_records = base::ifelse(name == 'Relativity', number_of_records, 0)) %>%
        plotly::plot_ly(height = height,
                        width = width,
                        showlegend = T) %>%
        plotly::add_trace(x = ~get(xaxisvariable),
                          y = ~value,
                          type="scatter",
                          mode="lines+markers",
                          color = ~get(facetorcolourtby)
                          #linetype = ~name,
                          #yaxis = "y2"
                          ) %>%
        plotly::add_bars(
          x = ~get(xaxisvariable),
          y = ~number_of_records,
          yaxis = 'y2',
          marker = list(color = '#dddddd',
                        line = list(width=0,
                                    color='black')),
          showlegend = FALSE
        ) %>%
        plotly::layout(title = base::paste(relativity_label, 'for', factor_name, 'interaction', 'coloured by', facetorcolourtby),
                       yaxis2 = list(side = 'right',
                                     title = 'Number of Records',
                                     showgrid = FALSE),
                       yaxis = list(overlaying='y2',
                                    side = 'left',
                                    title = relativity_label, #relativity_label
                                    showgrid = TRUE),
                       xaxis = list(title = xaxisvariable),
                       legend = list(orientation = "h",
                                     xanchor = "center",
                                     x=0.5,
                                     y=-0.2),
                       autosize = TRUE,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "ctsmain"){
  # Continuous Variables -----------------------------------------------------
  # Add Splined Variables???
  # prep the data
  plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-percentile_to_cut), na.rm = T),length.out =100),
                              relativity_value = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot), 'Relativity'))) %>%
    dplyr::mutate(feature_relativity = var_range*relativity_value)

  # plot density and relativity
  fit <- stats::density(dplyr::pull(dplyr::select(training_data, all_of(feature_to_plot))))
  p_return <- plotly::plot_ly(plot_data,
                       height = height,
                       width = width) %>%
    plotly::add_trace(x = ~var_range,
                      y = ~feature_relativity,
                      type="scatter",
                      mode="lines",
                      name = relativity_label,
                      line = list(color = 'black', width = 4),
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
                                title = 'Density',
                                zeroline = FALSE),
                   yaxis2 = list(side = 'left',
                                 title = relativity_label,
                                 showgrid = F,
                                 zeroline = FALSE,
                                 overlaying = 'y'),
                   legend = list(orientation = "h",
                                 y = -0.2,
                                 x = 0.32,
                                 title = ''),
                   xaxis = list(title = feature_to_plot,
                                zeroline = FALSE),
                   title = base::paste(relativity_label, 'for', feature_to_plot),
                   autosize = T,
                   margin = list(b = 50, l = 50, r=80))
  return(p_return)
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factorfactorinteraction"){
    # Filter to the ineraction we are plotting
    plot_data <- complete_factor_summary_df %>%
      dplyr::filter(Variable == feature_to_plot)

    # Get the number of records in each category
    factor_name <- base::unique(dplyr::pull(dplyr::select(plot_data, Variable)))

    # up to here remove if statemnts and change to plot data
    if (base::grepl(":", factor_name) == T){
      ivariable1 <- base::unlist(base::strsplit(factor_name, ':'))[1]
      ivariable2 <- base::unlist(base::strsplit(factor_name, ':'))[2]
      # For categorical categorical
      if ((class(dplyr::pull(dplyr::select(training_data, ivariable1))) %in% c('factor', 'character')) & ((class(dplyr::pull(dplyr::select(training_data, ivariable2))) %in% c('factor', 'character')))){
        count_df <- dplyr::select(training_data, c(ivariable1,ivariable2)) %>%
          dplyr::group_by_at(c(ivariable1,ivariable2)) %>%
          dplyr::summarise(number_of_records = dplyr::n(), .groups = 'drop') %>%
          dplyr::ungroup()
        count_df <- count_df %>%
          dplyr::mutate(Variable = factor_name) %>%
          dplyr::mutate(Level = base::paste0(base::get(ivariable1),':',base::get(ivariable2))) %>%
          dplyr::select(.,c('Level', 'number_of_records', 'Variable'))
        count_df_all <- base::rbind(count_df_all,count_df)
      }

  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factorandctsinteraction"){
    #cts cts interaction???? maybe same as continous but we need to fiddle with the density????
  }
  # cts
}

