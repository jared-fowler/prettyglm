test_that("feature is matched exactly, not by regex (A2)", {
  tt_cat <- tt %>% dplyr::mutate(Age_Cat = cut3(Age, g = 5)) %>% dplyr::relocate(Age_Cat)
  p <- one_way_ave('Age', tt_cat, 'Survived', m_main)
  # continuous branch buckets the feature, so there are far fewer than the 89 unique ages
  expect_lt(length(build_traces(p)[[1]]$x), 40)
  expect_error(one_way_ave('Fare.', tt, 'Survived', m_main), "not a column in data_set")
})

test_that("NA target rows are dropped with a warning (A8)", {
  tt_na <- tt
  tt_na$Survived[1:20] <- NA
  expect_warning(p <- one_way_ave('Pclass', tt_na, 'Survived', m_main), "Dropping 20 rows")
  expect_true('Actual' %in% trace_names(p) || any(grepl('Actual', trace_names(p))))
  expect_false(any_all_na_y(p))
})

test_that("yes/no factor target plots the actual line (A10)", {
  bank_f <- bank_data %>% dplyr::mutate(dplyr::across(c(job, marital), factor)) # y is already a no/yes factor
  m <- stats::glm(y ~ job + marital, data = bank_f, family = binomial)
  expect_warning(p <- one_way_ave('marital', bank_f, 'y', m), "coding 'yes' as 1")
  expect_false(any_all_na_y(p))
})

test_that("data_set defaults to the model data (A11)", {
  expect_s3_class(one_way_ave('Pclass', target_variable = 'Survived', model_object = m_main), 'plotly')
  expect_error(one_way_ave('Pclass', target_variable = 'Survived'), "Supply either data_set or model_object")
})

test_that("ordered factor features plot (A12)", {
  tt_ord <- tt %>% dplyr::mutate(Pclass = factor(Pclass, ordered = TRUE))
  expect_s3_class(one_way_ave('Pclass', tt_ord, 'Survived', plot_type = 'actuals'), 'plotly')
})

test_that("integers with many unique values are bucketed as continuous (A13)", {
  tt_int <- tt %>% dplyr::mutate(Age = as.integer(round(Age)))
  p <- one_way_ave('Age', tt_int, 'Survived', plot_type = 'actuals', number_of_buckets = 10)
  expect_lte(length(unique(build_traces(p)[[1]]$x)), 10)
  # few unique values stay categorical
  p2 <- one_way_ave('SibSp', tt, 'Survived', plot_type = 'actuals')
  expect_equal(length(unique(build_traces(p2)[[1]]$x)), length(unique(tt$SibSp)))
})
