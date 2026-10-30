test_that("lm models work (A5)", {
  expect_s3_class(actual_expected_bucketed('Fare', m_lm), 'plotly')
})

test_that("NA target rows are dropped with a warning (A8)", {
  tt_na <- tt
  tt_na$Survived[1:20] <- NA
  expect_warning(out <- actual_expected_bucketed('Survived', m_main, data_set = tt_na, return_data = TRUE), "Dropping 20 rows")
  expect_false(any(is.na(out$Average_value)))
})
