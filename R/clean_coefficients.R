#' @title clean_coefficients
#'
#' @description Processing to split out base levels and add variable importance to each term. Directly inspired by `tidycat::tidy_categorical()`, modified for use in prettyglm.
#'
#' @param d A data frame \code{\link[tibble]{tibble}} output from \code{\link[broom]{tidy.lm}}; with one row for each term in the regression, including column `term`
#' @param m A model object \code{\link[stats]{glm}}
#'
#' @return Expanded \code{\link[tibble]{tibble}} from the version passed to `d` including additional columns:
#' \item{variable}{The name of the variable that the regression term belongs to.}
#' \item{level}{The level of the categorical variable that the regression term belongs to. Will be an the term name for numeric variables.}
#' @seealso \code{\link[broom]{tidy.lm}}
#'
#' @author Jared Fowler, Guy J. Abel
#'
#' @export
#' @importFrom tibble "enframe"
#' @importFrom vip "vi"
#' @importFrom stringr "str_remove"
#' @importFrom forcats "fct_inorder"
#' @importFrom tidycat "factor_regex"
#' @import dplyr

clean_coefficients <- function(d = NULL, m  = NULL, vimethod = 'model'){
  # add Mat Ls formula fix

  # Global varaible notes fix
  name <- NULL
  level <- NULL
  variable <- NULL
  Importance <- NULL
  Sign <- NULL
  est <- NULL
  effect <- NULL
  estimate <- NULL
  std.error <- NULL
  relativity <- NULL

  # Extract model object if parsnip object
  if (any(class(m) == 'model_fit') == TRUE) m <- m$fit else m <- m

  # Extract model object if workflow object
  if (any(class(m) == 'workflow') == TRUE) m <- m$fit$fit$fit else m <- m

  #Split terms and get base levels
  x <- m %>%
    stats::dummy.coef() %>%
    base::unlist() %>%
    tibble::enframe(value = "est") %>%
    dplyr::mutate(
      variable = stringr::str_extract(string = name, pattern = tidycat::factor_regex(m)),
      level = stringr::str_remove(string = name, pattern = tidycat::factor_regex(m)),
      level = stringr::str_remove(string = level, pattern = "^[.]"),
      level = forcats::fct_inorder(level),
      effect = ifelse(test = stringr::str_detect(string = variable, pattern = ":"),
                      yes = base::ifelse(stringr::str_detect(string = level, pattern = ":", negate = TRUE),
                                         yes = "factorandctsinteraction",
                                         no = "otherinteraction"),
                      no = "main"))

  # count rows to to help categorise effects
  number_of_rows <- x %>%  dplyr::group_by(variable) %>% dplyr::summarise(number_of_rows=dplyr::n(), .groups = "drop")
  x <- x %>%
    dplyr::left_join(number_of_rows, by = "variable") %>%
    dplyr::mutate(effect = base::ifelse(effect == 'otherinteraction',
                                        yes = base::ifelse(number_of_rows == 1,
                                                           yes = 'ctsctsinteraction',
                                                           no = 'factorfactorinteraction'),
                                        no = effect)) %>%
    dplyr::mutate(effect = base::ifelse(effect == 'main',
                                        yes = base::ifelse(variable == level,
                                                           yes = 'ctsmain',
                                                           no = 'factormain'),
                                        no = effect))

  # Re-create term field for join
  term_record <- vector(mode = "list", length = length(x$variable))
  for(i in 1:length(x$variable)){
    if (x$variable[i] == '(Intercept)'){
      term_record[[i]] <- '(Intercept)'
    } else if (base::grepl(x = x$variable[i], pattern=':', fixed = TRUE)){
      # Re-create term name for any number of interacted variables
      termname <- ''
      if (x$effect[i] == 'factorandctsinteraction'){
        # factors and cts need to be handled different
        # not built for more than one interaction
        lev <- base::unlist(base::strsplit(as.character(x$level[i]), ':'))
        var1 <- base::unlist(base::strsplit(x$variable[i], ':'))[1] #hopefully always factor 1
        var2 <- base::unlist(base::strsplit(x$variable[i], ':'))[2]
        termname <- base::paste0(var1,lev, ':', var2)
      } else{
        for (k in 1:base::length(base::unlist(base::strsplit(as.character(x$level[i]), ':')))){
          lev <- base::unlist(base::strsplit(as.character(x$level[i]), ':'))[k]
          var <- base::unlist(base::strsplit(x$variable[i], ':'))[k]
          if (base::nchar(termname)==0){
            termname <- base::paste0(termname, var, lev)
          } else{
            termname <- base::paste0(termname, ':', var, lev)
          }
        }
      }
      # Assign term name to correct value in a list
      term_record[[i]] <- termname
    } else if (x$variable[i] %in% names(which(attr(m$terms,"dataClasses") == 'numeric'))){
      # Numeric variables have the same term name as their variable
      term_record[[i]] <- x$variable[i]
    } else{
      #If not an interaction of an numeric it must be a factor with multiple levels
      term_record[[i]] <- base::paste0(x$variable[i], x$level[i])
    }
  }
  x$term <- base::unlist(term_record)
  x <- x %>%
    dplyr::mutate(term = base::ifelse(effect == 'ctsctsinteraction',
                                      yes = variable,
                                      no = term))

  # Calculate variable importance and add to summary
  v <- vip::vi(m, method = vimethod)
  x <- dplyr::left_join(x, v, by = c('term' = 'Variable')) %>%
    dplyr::mutate(Importance = base::ifelse(is.na(Importance), 0 , Importance),
                  Sign = base::ifelse(is.na(Sign), 'NEU' , Sign))

  # Select columns we want in the output
  x <- x %>%
    dplyr::select(-name, -est) %>%
    dplyr::left_join(d, by = "term") %>%
    dplyr::select(c(term, variable, level, effect, Importance, Sign, estimate, std.error, statistic, p.value))

  return(x)
}

