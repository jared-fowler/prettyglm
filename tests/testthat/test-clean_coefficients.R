test_that("conf.int = TRUE keeps confidence intervals (A1)", {
  p_file <- pretty_coefficients(m_main, conf.int = TRUE, return_data = TRUE)
  expect_true(all(c('Conf.low', 'Conf.high') %in% names(p_file)))
  expect_false(any(is.na(p_file$Conf.low)))
})
