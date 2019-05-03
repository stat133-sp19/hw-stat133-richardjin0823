context("Check Binomial Function's inputs and outputs")

test_that("checks that Binomial Choose fails if k is greater than n",{
  expect_error(bin_choose (n = 5, k = 6))
  expect_error(bin_choose (n = 0, k = 1))
  expect_error(bin_choose (n = -1, k = 0))
})

test_that("checks that Binomial Choose if valid if n is greater than or equal to k and that the outcome is correct",{
  expect_equal(bin_choose (n = 6, k = 6), 1)
  expect_equal(bin_choose (n = 6, k = 3), 20)
  expect_equal(bin_choose (n = 6, k = 2), 15)
  expect_equal(bin_choose (n = 6, k = 4), 15)
  expect_equal(bin_choose (n = 6, k = 0), 1)
  expect_equal(bin_choose (n = 0, k = 0), 1)
})

test_that("checks that Binomial Choose can work even if k is a vector with multiple elements",{
  expect_length(bin_choose (n = 6, k = 1:3), length(1:3))
  expect_length(bin_choose (n = 6, k = c(1,5)), 2)
})

test_that("checks that Binomial Probability fails if success is not valid",{
  expect_error(bin_probability (success = 2.1, trials = 5, prob = 0.5))
  expect_error(bin_probability (success = 6, trials = 5, prob = 0.5))
  expect_error(bin_probability (success = -1, trials = 5, prob = 0.5))
  expect_error(bin_probability (success = "g", trials = 5, prob = 0.5))
})

test_that("checks that Binomial Probability fails if trials is not valid",{
  expect_error(bin_probability (success = 2, trials = 1.1, prob = 0.5))
  expect_error(bin_probability (success = 2, trials = -1, prob = 0.5))
  expect_error(bin_probability (success = 2, trials = "g", prob = 0.5))
})

test_that("checks that Binomial Probability fails if prob is not valid",{
  expect_error(bin_probability (success = 2, trials = 5, prob = 1.1))
  expect_error(bin_probability (success = 2, trials = 5, prob = -0.1))
  expect_error(bin_probability (success = 2, trials = 5, prob = "a"))
})

test_that("checks that Binomial Probability yields the right results",{
  expect_equal(bin_probability (success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability (success = 0, trials = 5, prob = 0.5), 0.03125)
  expect_equal(bin_probability (success = 0, trials = 0, prob = 0.5), 1)
})

test_that("checks that Binomial Distribution fails if trials and prob are not valid",{
  expect_error(bin_distribution(trials = 5.4, prob = 0.5))
  expect_error(bin_distribution(trials = -1, prob = 0.5))
  expect_error(bin_distribution(trials = 5, prob = 1.1))
  expect_error(bin_distribution(trials = 5.4, prob = -0.1))
  expect_error(bin_distribution(trials = 5, prob = c(1,1)))
})

test_that("checks that Binomial Distribution Probabilities sum to 1",{
  expect_equal(sum(bin_distribution(trials = 5, prob = 0.5)$probability), 1)
  expect_equal(sum(bin_distribution(trials = 10, prob = 0.2)$probability), 1)
  expect_equal(sum(bin_distribution(trials = 1, prob = 0.5)$probability), 1)
  expect_equal(sum(bin_distribution(trials = 0, prob = 0.5)$probability), 1)
})

test_that("checks that Binomial Distribution has the right number of terms",{
  expect_equal(length(bin_distribution(trials = 10, prob = 0.5)$probability), 11)
  expect_equal(length(bin_distribution(trials = 5, prob = 0.2)$probability), 6)
  expect_equal(length(bin_distribution(trials = 1, prob = 0.5)$probability), 2)
  expect_equal(length(bin_distribution(trials = 0, prob = 0.5)$probability), 1)
})

test_that("checks that Binomial Cumulative fails if trials and prob are not valid",{
  expect_error(bin_cumulative(trials = 5.4, prob = 0.5))
  expect_error(bin_cumulative(trials = -1, prob = 0.5))
  expect_error(bin_cumulative(trials = 5, prob = 1.1))
  expect_error(bin_cumulative(trials = 5.4, prob = -0.1))
})

test_that("checks that Binomial Cumulative has the right number of terms",{
  expect_equal(length(bin_cumulative(trials = 10, prob = 0.5)$cumulative), 11)
  expect_equal(length(bin_cumulative(trials = 5, prob = 0.2)$cumulative), 6)
  expect_equal(length(bin_cumulative(trials = 1, prob = 0.5)$cumulative), 2)
  expect_equal(length(bin_cumulative(trials = 0, prob = 0.5)$cumulative), 1)
})

test_that("checks that in Binomal Cumulative, last element of cumulative column equals 1",{
  expect_equal(bin_cumulative(trials = 10, prob = 0.5)$cumulative[11], 1)
  expect_equal(bin_cumulative(trials = 5, prob = 0.2)$cumulative[6], 1)
  expect_equal(bin_cumulative(trials = 1, prob = 0.5)$cumulative[2], 1)
  expect_equal(bin_cumulative(trials = 0, prob = 0.5)$cumulative[1], 1)
})










