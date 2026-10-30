test_that("a two level non-numeric factor target is coded like glm (A10)", {
  y <- factor(c('no', 'yes', 'yes', 'no'))
  expect_warning(out <- target_to_numeric(y), "coding 'yes' as 1")
  expect_equal(out, c(0, 1, 1, 0))
})

test_that("numeric-looking factor targets keep their values", {
  expect_equal(target_to_numeric(factor(c('0', '1', '1'))), c(0, 1, 1))
  expect_equal(target_to_numeric(c(2.5, 3)), c(2.5, 3))
})

test_that("factor targets with more than two non-numeric levels error", {
  expect_error(target_to_numeric(factor(c('a', 'b', 'c'))), "two level factor")
})

test_that("lm data is found from the call (A5)", {
  expect_true(is.data.frame(get_model_data(m_lm)))
})

test_that("a model fit without data = errors clearly (A6)", {
  m_nodata <- with(tt, stats::glm(Survived ~ Pclass + Age, family = binomial))
  expect_error(get_model_data(m_nodata), "data = argument")
})
