#' @title pretty_coefficients
#'
#' @description Creates a pretty kable of model coefficients including coefficient base levels.
#'
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats:glm]{stats::glm()}, \link[stats:lm]{stats::lm()},  \link[parsnip:linear_reg()]{parsnip::linear_reg()}, \link[parsnip:logistic_reg()]{parsnip::logistic_reg()} or  \link[poissonreg:poisson_reg()]{poissonreg::poisson_reg()}
#' @param conf.int Set to TRUE to include confidence intervals in summary table. Warning, can be computationally expensive.
#' @param relativity_transform String of the function to be applied to the model estimate to calculate the relativity, for example: 'exp(estimate)-1'. Default is for relativity to be excluded from output.
#' @param type_iii Type III statistical test to perform. Default is none. Options are 'Wald' or 'LR'. Warning 'LR' can be computationally expensive. Test performed via \link[car:Anova()]{car::Anova()}
#' @param return_data Set to TRUE to return \link[base:data.frame]{base::data.frame()} instead of creating \link[knitr:kable]{knitr::kable()}.
#'
#' @return \link[knitr:kable]{knitr::kable()} if return_data = FALSE. \link[base:data.frame]{base::data.frame()} if return_data = TRUE.
#'
#' @examples
#' \dontrun{

#' }
#' @export
#' @importFrom tibble "tibble"
#' @importFrom broom "tidy"
#' @importFrom tidyselect "all_of"
#' @importFrom stringr "str_replace"
#' @importFrom knitr "kable"
#' @importFrom kableExtra "kable_styling"
#' @importFrom kableExtra "row_spec"
#' @importFrom kableExtra "cell_spec"
#' @importFrom kableExtra "collapse_rows"
#' @importFrom kableExtra "footnote"
#' @importFrom formattable "normalize_bar"
#' @importFrom workflows "pull_workflow_fit"
#' @importFrom car "Anova"
#' @import dplyr
#'

pretty_coefficients <- function(model_object, relativity_transform = NULL, type_iii = NULL, conf.int = FALSE, return_data = FALSE){

  #TODO: fix NA in goodness of fit metrics

  # if model object is a workflow, pull the model fit
  if (any(class(model_object) == 'workflow') == TRUE){
    model_object <- model_object$fit$fit$fit
  }

  # if model object is a parsnip model, pull the model fit
  if (any(class(model_object) == 'model_fit') == TRUE){
    model_object <- model_object$fit
  }

  # tidy coefficients
  model_tidy_df <- broom::tidy(model_object, conf.int=conf.int)
  tidy_coef <- prettyglm::clean_coefficients(d=model_tidy_df, m=model_object)

  # replace NAs with 0
  tidy_coef <- tidy_coef %>%  dplyr::mutate(estimate = ifelse(is.na(estimate), 0, estimate),
                                            std.error = ifelse(is.na(std.error), 0, std.error))
  # add relativity
  if (base::is.null(relativity_transform) != TRUE){
    base::eval(base::parse(text = base::paste('relativity <- function(estimate) { return(' , relativity_transform , ')}', sep='')))
    tidy_coef <- tidy_coef %>%
      dplyr::mutate(relativity = (relativity(estimate)))
  }

  # confidence interval and relativity formatting
  if (conf.int == T){
    tidy_coef <- tidy_coef %>%
      dplyr::mutate(conf.low = ifelse(is.na(conf.low), 0, conf.low),
                    conf.high = ifelse(is.na(conf.high), 0, conf.high))
    if (base::is.null(relativity_transform) != TRUE){
      tidy_coef <- tidy_coef %>%
        dplyr::select(c(variable, level, Importance, estimate, std.error, conf.low, conf.high, relativity, p.value)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.Error = std.error,
                      Conf.low = conf.low,
                      Conf.high = conf.high,
                      Relativity = relativity,
                      P.Value= p.value)
    } else{
      tidy_coef <- tidy_coef %>%
        dplyr::select(c(variable, level, Importance, estimate, std.error, conf.low, conf.high, p.value)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.Error = std.error,
                      Conf.low = conf.low,
                      Conf.high = conf.high,
                      P.Value= p.value)
      }
  } else {
    if (base::is.null(relativity_transform) != TRUE){
      tidy_coef <- tidy_coef %>% dplyr::select(c(variable, level, Importance, estimate, std.error, relativity, p.value)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.error = std.error,
                      Relativity = relativity,
                      P.Value= p.value)
    } else {
      tidy_coef <- tidy_coef %>% dplyr::select(c(variable, level, Importance, estimate, std.error, p.value)) %>%
        dplyr::rename(Variable = variable,
                      Level = level,
                      Estimate = estimate,
                      Std.error = std.error,
                      P.Value= p.value)
    }
  }

  # add type III test
  if (is.null(type_iii) == F){
    if(!(type_iii %in% c('Wald', 'LR'))) {stop('type_iii must be either: "Wald" or "LR"')}
      term_p_values <- broom::tidy(car::Anova(model_object, type = 'III', test.statistic=type_iii)) %>%
        dplyr::select(c('term', 'p.value')) %>%
        dplyr::rename('Type.III.P.Value' = 'p.value')
      tidy_coef <- dplyr::inner_join(tidy_coef, term_p_values, by = c('Variable' = 'term'))
  }

  # return desired output
  if (return_data == F){

    # Extract goodness of fit metrics
    if (any(class(model_object) == 'model_fit') == TRUE){
      aic_print <- base::ifelse(base::is.null(model_object$fit$aic), NA, model_object$fit$aic)
      deviance_print <- base::ifelse(base::is.null(model_object$fit$deviance), NA, model_object$fit$deviance)
      null_deviance_print <- base::ifelse(base::is.null(model_object$fit$null.deviance ), NA, model_object$fit$null.deviance)
    } else{
      aic_print <- base::ifelse(base::is.null(model_object$aic), NA, model_object$aic)
      deviance_print <- base::ifelse(base::is.null(model_object$deviance ), NA, model_object$deviance)
      null_deviance_print <- base::ifelse(base::is.null(model_object$null.deviance), NA, model_object$null.deviance)
    }

    # Create a nice kable output of coefficients
    kable_df <- tidy_coef
    kable_df$P.Value = kableExtra::cell_spec(base::round(kable_df$P.Value,5), background  = ifelse(is.na(kable_df$P.Value) |  kable_df$P.Value < 0.05, "#black", "#F08080"))
    kable_df$Importance <- formattable::normalize_bar(color  = "lightgrey")(base::round(kable_df$Importance,1))
    kable_df$Importance <- stringr::str_replace(kable_df$Importance, 'rtl;', 'ltr;')
    if(is.null(type_iii)){
      kable_table <- knitr::kable(kable_df,
                                  escape = F,
                                  booktabs = T,
                                  #caption='Model estimates and p-values',
                                  align = c("l","l","l","r", "r", "r", "r", "r"))%>%
        kableExtra::kable_styling() %>%
        kableExtra::collapse_rows(columns = 1) %>%
        kableExtra::footnote(general = base::paste(' AIC:',
                                                   base::round(aic_print,1),
                                                   ', Devience :',
                                                   base::round(deviance_print,1),
                                                   ', Null Devience: ',
                                                   base::round(null_deviance_print,1)),
                             general_title = 'Goodness-of-Fit:',
                             footnote_as_chunk = T,
                             title_format = c("italic", "underline"))

    } else{
      kable_df$Type.III.P.Value = kableExtra::cell_spec(base::round(kable_df$Type.III.P.Value,5), background  = ifelse(is.na(kable_df$Type.III.P.Value) |  kable_df$Type.III.P.Value < 0.05, "#black", "#F08080"))
      kable_table <- knitr::kable(kable_df,
                                  escape = F,
                                  booktabs = T,
                                  #caption='Model estimates and p-values',
                                  align = c("l","l","l","r", "r", "r", "r", "r", "r"))%>%
        kableExtra::kable_styling() %>%
        kableExtra::collapse_rows(columns = c(1, base::ncol(kable_df)), target = 1) %>%
        kableExtra::footnote(general = base::paste(' AIC:',
                                                   base::round(aic_print,1),
                                                   ', Devience :',
                                                   base::round(deviance_print,1),
                                                   ', Null Devience: ',
                                                   base::round(null_deviance_print,1)),
                             general_title = 'Goodness-of-Fit:',
                             footnote_as_chunk = T,
                             title_format = c("italic", "underline"))
    }
    return(kable_table)
  } else{
    # If kable false, return dataframe
    return(tidy_coef)
  }
}
