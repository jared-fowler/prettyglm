
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prettyglm <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/prettyglm)](https://cran.r-project.org/package=prettyglm)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/prettyglm)](https://cran.r-project.org/package=prettyglm)
[![R build
status](https://github.com/jared-fowler/prettyglm/workflows/R-CMD-check/badge.svg)](https://github.com/jared-fowler/prettyglm/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

One of the main advantages of using Generalised Linear Models is their
interpretability. The goal of prettyglm is to provide a set of functions
which easily create beautiful coefficient summaries which can readily be
shared and explained.

## Forword

`prettyglm` was created to solve some common faced when building
Generalised Linear Models, such as displaying categorical base levels,
and visualizing the number of records in each category on a duel axis.
Since then a number of other functions which are useful when fitting
glms have been added.

If you don’t find the function you are looking for here consider
checking out some other great packages which help visualize the output
from glms: - `tidycat` - `jtools`

## Installation

You can install the latest CRAN release with:

``` r
install.packages('prettyglm')
```

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
library(Hmisc)
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
  dplyr::mutate(age_cat = Hmisc::cut2(age, g=30, levels.mean=T)) %>% #cut age variable into categories
  dplyr::mutate(T_DEPOSIT = as.factor(base::ifelse(y=='yes',1,0))) #convert target to 0 and 1 for performance plots
```

### Building a glm

For this example we will build a glm using `stats::glm()`, however
`prettyglm` also supports parsnip and workflow model objects which use
the `glm` model engine.

Note the point of this README is not to create the best model, but to
highlight the features of this package.

``` r
deposit_model <- stats::glm(T_DEPOSIT ~ job +
                                marital +
                                education +
                                default +
                                loan +
                                age_cat,
                             data = bank_data,
                             family = binomial)
```

### Visualising Fitted Model Coefficients

#### Create table of model coefficients with `pretty_coefficients()`

  - `pretty_coefficients()` automatically includes categorical variable
    base levels.
  - You can complete a type III test on the coefficients by specifying a
    `type_iii` argument.
  - You can include a “relativity” column in the output by including a
    `relativity_transform` input. (Note “relativity” is sometimes
    referred to as “likelihood” or “odds-ratio”, you can change the
    title of this column with the `relativity_label` input.)
  - You can return the data set instead of `kable` but setting
    `Return_Data = TRUE`

<!-- end list -->

``` r
pretty_coefficients(deposit_model, type_iii = 'Wald')
```

<p align="center">

<img src= './man/figures/full_table_3.png' height="1200" width = "600" align="center"/>

</p>

#### Create plots of the model relativities with `pretty_relativities()`

  - A model relativity is a transform of the model estimate. By default
    `pretty_relativities()` uses ‘exp(estimate)-1’ which is useful for
    GLM’s which use a log or logit link function.
  - `pretty_relativities()` automatically extracts the training data
    from the model object and plots th number of records on the second y
    axis.
  - If the variable you are plotting is a continuous variable which has
    been split into buckets, you can set `plot_factor_as_numeric` equal
    to `TRUE` to plot the x axis on a numerical scale.

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

### Model Performance

#### Plot overall actual vs expected by predicted band with (still in development) `actual_expected_bucketed`

``` r
actual_expected_bucketed(Target_Variable = 'T_DEPOSIT',
                         Model_Object = deposit_model,
                         Data_Set = bank_data)
```

<p align="center">

<img src= './man/figures/bank_overall_ave.png' height="500" align="center"/>

</p>

### Support My Work

[![“Buy Me A
Coffee”](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/tictap)
