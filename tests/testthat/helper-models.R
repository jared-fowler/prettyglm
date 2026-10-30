# Shared fixtures for the test suite (loaded automatically by testthat)
data("titanic")
data("bank")

tt <- titanic %>%
  dplyr::mutate(dplyr::across(c(Pclass, Sex, Embarked), factor)) %>%
  dplyr::mutate(
    Age = ifelse(is.na(Age), 30, Age),
    Age_0_25 = splineit(Age, 0, 25),
    Age_25_50 = splineit(Age, 25, 50),
    Age_50_120 = splineit(Age, 50, 120)
  )

m_main <- stats::glm(Survived ~ Pclass + Sex + Age + Fare, data = tt, family = binomial)
m_inter <- stats::glm(Survived ~ Pclass:Fare + Embarked:Sex + Sex, data = tt, family = binomial)
m_lm <- stats::lm(Fare ~ Pclass + Age + Sex, data = tt)

# Plot structure helpers -------------------------------------------------------------------------
build_traces <- function(p) plotly::plotly_build(p)$x$data
trace_names <- function(p) vapply(build_traces(p), function(t) as.character(t$name %||% ''), character(1))
any_all_na_y <- function(p) any(vapply(build_traces(p), function(t) length(t$y) > 0 && all(is.na(t$y)), logical(1)))
`%||%` <- function(a, b) if (is.null(a)) b else a
axis_title <- function(p, axis = 'xaxis') {
  t <- plotly::plotly_build(p)$x$layout[[axis]]$title
  if (is.list(t)) t$text else t
}
