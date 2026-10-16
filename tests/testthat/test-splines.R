data("titanic")

titanic_spline <- titanic %>%
  dplyr::mutate(
    Age_0_25 = splineit(Age, 0, 25),
    Age_25_50 = splineit(Age, 25, 50),
    Age_50_120 = splineit(Age, 50, 120)
  )

# All-numeric model: dummy.coef() does not repeat term names when there are no factors
spline_model <- stats::glm(
  Survived ~ Age_0_25 + Age_25_50 + Age_50_120,
  data = titanic_spline,
  family = binomial(link = 'logit')
)

test_that("splines are detected in a model with no factor terms", {
  p_file <- pretty_coefficients(spline_model, return_data = TRUE, spline_seperator = '_')
  expect_true(all(p_file$Effect[p_file$Variable == 'Age'] == 'ctsspline'))
  expect_equal(sum(p_file$Variable == 'Age'), 3)
})

test_that("pretty_relativities plots a spline from an all-numeric model", {
  p <- pretty_relativities(feature_to_plot = 'Age', model_object = spline_model, spline_seperator = '_')
  expect_s3_class(p, 'plotly')
})

test_that("spline_parts handles separators inside the base name", {
  parts <- spline_parts(c('Fare_paid_0_250'), '_')
  expect_equal(parts$variable[1], 'Fare_paid')
  expect_equal(parts$min[1], 0)
  expect_equal(parts$max[1], 250)
  expect_equal(spline_parts('Age.0.25', '.')$variable, 'Age')
})
