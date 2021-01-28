
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prettyglm <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/jared-fowler/prettyglm/workflows/R-CMD-check/badge.svg)](https://github.com/jared-fowler/prettyglm/actions)
<!-- badges: end -->

The goal of prettyglm is to easily create beautiful coefficient
summaries for Generalised Linear Models.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jared-fowler/prettyglm")
```

## A Simple Example

To explore the functionality of prettyglm we will use the titanic data
set to perform logistic regression. This data set was sourced from
<https://www.kaggle.com/c/titanic/data> and contains information about
passengers aboard the titanic, and a target variable which indicates if
they survived.

``` r
library(prettyglm)
library(dplyr)
data("titanic")
```

### Pre-processing

A critical step for this package to work well is to **set all
categorical predictors as factors**.

``` r
# Easiest way to convert multiple columns to a factor.
columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin', 
                       'Embarked',
                       'Cabintype')
titanic  <- titanic  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.)))
```

### Building a glm

For this example we will build a glm using `stats::glm()`, but
`prettyglm` also supports parsnip and workflow model objects which use
the `glm` model engine.

``` r
survival_model <- stats::glm(Survived ~ Pclass + Sex + Age + Fare + Embarked + SibSp + Parch + Cabintype, 
                             data = titanic, 
                             family = binomial(link = 'logit'))
```

### Create table of model coefficients with `pretty_coefficients()`

  - `pretty_coefficients()` automatically includes categorical base
    levels.
  - You complete a type III test on the coefficients by specifying a
    `type_iii` argument.
  - You can return the data set instead of `kable` but setting
    `Return_Data = TRUE`

<!-- end list -->

``` r
pretty_coefficients(survival_model, type_iii = 'Wald')
```

<p align="center">

<img src= './man/figures/result.png' height="1000" align="center"/>

</p>

### Create plots of the model relativities with `pretty_relativities()`

  - A model relativity is a transform of the model estimate. By default
    `pretty_relativities()` uses ‘exp(estimate)-1’ which is useful for
    GLM’s which use a log or logit link function.
  - `pretty_relativities()` automatically extracts the training data
    from the model objects and plots the relativities and the number of
    records in the same plot.

<!-- end list -->

``` r
pretty_relativities(feature_to_plot = 'Embarked',
                    model_object = survival_model)
```

<p align="center">

<img src= './man/figures/rel_plot1.png' height="500" align="center"/>

</p>
