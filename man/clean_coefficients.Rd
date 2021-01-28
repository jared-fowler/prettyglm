% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_coefficients.R
\name{clean_coefficients}
\alias{clean_coefficients}
\title{clean_coefficients}
\usage{
clean_coefficients(d = NULL, m = NULL)
}
\arguments{
\item{d}{A data frame \link[tibble:tibble]{tibble::tibble()} output from \link[broom:tidy.lm]{broom::tidy.lm()}; with one row for each term in the regression, including column `term`}

\item{m}{A model object \link[stats:glm]{glm()}}
}
\value{
Expanded \link[tibble:tibble]{tibble::tibble()} from the version passed to `d` including additional columns:
\item{variable}{The name of the variable that the regression term belongs to.}
\item{level}{The level of the categorical variable that the regression term belongs to. Will be an the term name for numeric variables.}
}
\description{
Processing to split out base levels and add variable importance to each term. Inspired by tidycat::tidy_categorical, modified for prettyglm.
}
\seealso{
\link[broom:tidy.lm]{broom::tidy.lm()}
}
\author{
Jared Fowler
}