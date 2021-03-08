
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prettyglm <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/jared-fowler/prettyglm/workflows/R-CMD-check/badge.svg)](https://github.com/jared-fowler/prettyglm/actions)
<!-- badges: end -->

One of the main advantages of using Generalised Linear Models is their
interpretability. The goal of prettyglm is to provide a set of functions
which easily create beautiful coefficient summaries which can readily be
shared and explained.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jared-fowler/prettyglm")
```

## A Simple Example

To explore the functionality of prettyglm we will use a data set sourced
from
<https://www.kaggle.com/volodymyrgavrysh/bank-marketing-campaigns-dataset>
which contains information about a Portugal banks marketing campaigns
results. The campaign was based mostly on direct phone calls, offering
clients a term deposit. The target variable `y` indicates if the client
agreed to place the deposit after the phone call.

``` r
library(prettyglm)
library(dplyr)
data("bank")
```

### Pre-processing

A critical step for this package to work well is to **set all
categorical predictors as factors**.

``` r
# Easiest way to convert multiple columns to a factor.
columns_to_factor <- c('job',
                       'marital',
                       'education',
                       'default',
                       'housing',
                       'loan')
bank_data  <- bank_data  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>% # multiple columns to factor
  dplyr::mutate(age_cat = Hmisc::cut2(age, g=30, levels.mean=T)) #cut age variable into categories
```

### Building a glm

For this example we will build a glm using `stats::glm()`, however
`prettyglm` also supports parsnip and workflow model objects which use
the `glm` model engine.

Note the point of this README is not to create the best model, but to
highlight the features of this package.

``` r
deposit_model <- stats::glm(y ~ job +
                                marital +
                                education +
                                default +
                                loan +
                                age_cat,
                             data = bank_data,
                             family = binomial)
```

### Create table of model coefficients with `pretty_coefficients()`

  - `pretty_coefficients()` automatically includes categorical variable
    base levels.
  - You can complete a type III test on the coefficients by specifying a
    `type_iii` argument.
  - You can return the data set instead of `kable` but setting
    `Return_Data = TRUE`

<!-- end list -->

``` r
pretty_coefficients(deposit_model, type_iii = 'Wald')
```

<p align="center">

<img src= './man/figures/full_table.png' height="1200" width = "600" align="center"/>

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
pretty_relativities(feature_to_plot = 'job',
                    model_object = deposit_model)
```

<p align="center">

<img src= './man/figures/job_rel.png' height="500" align="center"/>

</p>

``` r
pretty_relativities(feature_to_plot = 'age_cat',
                    model_object = deposit_model,
                    plot_factor_as_numeric = T)
```

<p align="center">

<img src= './man/figures/age_rels.png' height="500" align="center"/>

</p>
