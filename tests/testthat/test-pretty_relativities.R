test_that("ordering accepts a vector of levels and appends missing ones (A3)", {
  p <- pretty_relativities('Pclass', m_main, ordering = c('3', '1'))
  expect_equal(levels(build_traces(p)[[1]]$x), c('3', '1', '2'))
})

test_that("bad ordering values error clearly (A3)", {
  expect_error(pretty_relativities('Pclass', m_main, ordering = 'garbage'), "ordering must be one of")
  expect_error(pretty_relativities('Pclass', m_main, ordering = c('3', '9')), "Levels not found: 9")
})

test_that("integer base column works in factor x spline interactions (A4)", {
  tt_int <- tt %>% dplyr::mutate(Age = as.integer(round(Age)))
  m <- stats::glm(Survived ~ Sex:Age_0_25 + Sex:Age_25_50 + Sex:Age_50_120, data = tt_int, family = binomial)
  for (type in c('colour', 'facet')) {
    p <- pretty_relativities('Sex:Age', m, spline_seperator = '_', iteractionplottype = type)
    expect_s3_class(p, 'plotly')
    expect_false(any_all_na_y(p))
  }
})

test_that("lm models plot (A5)", {
  expect_s3_class(pretty_relativities('Pclass', m_lm, relativity_transform = 'estimate'), 'plotly')
  expect_s3_class(pretty_relativities('Age', m_lm, relativity_transform = 'estimate'), 'plotly')
})

test_that("character predictors error clearly (A7)", {
  tt_chr <- tt %>% dplyr::mutate(Pclass = as.character(Pclass))
  m <- stats::glm(Survived ~ Pclass + Age, data = tt_chr, family = binomial)
  expect_error(pretty_relativities('Pclass', m), "Character predictors are not supported: Pclass")
})

test_that("spline without the raw base column errors clearly (A9)", {
  tt_noage <- tt %>% dplyr::select(-Age)
  m <- stats::glm(Survived ~ Age_0_25 + Age_25_50 + Age_50_120, data = tt_noage, family = binomial)
  expect_error(pretty_relativities('Age', m, spline_seperator = '_'), "raw column Age must be in the training data")
})

test_that("ordered factor predictors error clearly (A12)", {
  tt_ord <- tt %>% dplyr::mutate(Pclass = factor(Pclass, ordered = TRUE))
  m <- stats::glm(Survived ~ Pclass + Age, data = tt_ord, family = binomial)
  expect_error(pretty_relativities('Pclass', m), "Ordered factor predictors are not supported")
})

test_that("factor x cts colour plot names traces by level and shows CI (A14)", {
  p <- suppressMessages(pretty_relativities('Pclass:Fare', m_inter, iteractionplottype = 'colour'))
  traces <- build_traces(p)
  expect_setequal(unique(trace_names(p)), c('1', '2', '3', 'Density'))
  expect_equal(sum(vapply(traces, function(t) identical(t$line$dash, 'dot'), logical(1))), 6)
  expect_equal(axis_title(p), 'Fare')
})

test_that("factor x cts colour plot without CI has one trace per level (A14)", {
  p <- suppressMessages(pretty_relativities('Pclass:Fare', m_inter, iteractionplottype = 'colour', plot_approx_ci = FALSE))
  expect_length(build_traces(p), 4)
})

test_that("factor x cts facet x axis is the continuous variable (A14)", {
  p <- pretty_relativities('Pclass:Fare', m_inter, iteractionplottype = 'facet')
  expect_equal(axis_title(p), 'Fare')
})

test_that("factor x factor colour plot shows CI grouped with each level (A14)", {
  p <- pretty_relativities('Embarked:Sex', m_inter, iteractionplottype = 'colour', facetorcolourby = 'Sex')
  traces <- build_traces(p)
  ci <- Filter(function(t) identical(t$line$dash, 'dot'), traces)
  expect_length(ci, 4)
  expect_setequal(vapply(ci, function(t) t$legendgroup, character(1)), c('female', 'male'))
})
