context("Check Private Auxiliary Function outputs")

test_that("checks that auxiliary mean yields the correct results",{
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(10, 1), 10)
  expect_equal(aux_mean(10, 0), 0)
  expect_equal(aux_mean(10, 0.5), 5)
})

test_that("checks that auxiliary variance yields the correct results",{
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(10, 0.5), 2.5)
  expect_equal(aux_variance(10, 1), 0)
  expect_equal(aux_variance(10, 0.7), 2.1)
})

test_that("checks that auxiliary mode yields the correct results",{
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(10, 0.45), 4)
  expect_equal(aux_mode(10, 0), 0)
  expect_equal(aux_mode(10, 0.9), 9)
})

test_that("checks that auxiliary skewness is equal to zero if prob = 0.5, less than zero if prob > 0.5, greater than zero if prob <0.5, and the absolute value of skewness becomes larger as prob approaches extremes",{
  expect_gt(aux_skewness(10, 0.3), 0)
  expect_gt(aux_skewness(10, 0.1), aux_skewness(10, 0.4))
  expect_equal(aux_skewness(10, 0.5), 0)
  expect_equal(aux_skewness(5, 0.5), 0)
  expect_lt(aux_skewness(10, 0.6), 0)
  expect_lt(aux_skewness(10, 0.9),aux_skewness(10, 0.6))
})

test_that("checks that auxiliary kurtosis yields the correct results for prob = 0.5, and correct ballpark for other probs",{
  expect_equal(aux_kurtosis(10, 0.5), -0.2)
  expect_equal(aux_kurtosis(5, 0.5), -0.4)
  expect_lt(aux_kurtosis(10, 0.3), 0)
  expect_lt(aux_kurtosis(10, 0.6), 0)
  expect_gt(aux_kurtosis(10, 0.9), 0)
  expect_equal(aux_kurtosis(10, 1), Inf)
})




