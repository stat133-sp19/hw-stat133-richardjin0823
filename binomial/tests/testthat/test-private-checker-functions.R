context("Check Private Checker Function arguments")

test_that("check_prob works with a number in between 0 and 1",{
  expect_true(check_prob(0))
  expect_true(check_prob(1))
  expect_true(check_prob(0.5))
})

test_that("check_prob fails with a number that is not between 0 and 1", {
  expect_error(check_prob(1.1))
  expect_error(check_prob(-0.1))
  expect_error(check_prob(10))
  expect_error(check_prob(-10))
})

test_that("check_prob fails with inputs that are not of length 1", {
  expect_error(check_prob(c(0.1,0.2)))
  expect_error(check_prob(c(0,1,0.5)))
  expect_error(check_prob(c(1,1,1,1,1,1)))
})

test_that("check_prob fails with non-numbers",{
  expect_error(check_prob("one"))
  expect_error(check_prob("probability"))
  expect_error(check_prob(TRUE))
})

test_that("check_trials fails with non-integers",{
  expect_error(check_trials(-5.2))
  expect_error(check_trials(-0.2))
  expect_error(check_trials(0.3))
  expect_error(check_trials(1.5))
  expect_error(check_trials(120.3))
})

test_that("check_trials fails with numbers that are negative",{
  expect_error(check_trials(-5.2))
  expect_error(check_trials(-0.2))
  expect_error(check_trials(-5))
  expect_error(check_trials(-1))
  expect_error(check_trials(-120))
})

test_that("check_trials works with numbers that are non-negative integers",{
  expect_true(check_trials(0))
  expect_true(check_trials(1))
  expect_true(check_trials(10))
  expect_true(check_trials(120))
})

test_that("check_trials fails with input lengths that are not 1",{
  expect_error(check_trials(c(1,2)))
  expect_error(check_trials(1:5))
  expect_error(check_trials(c(1:10)))
})

test_that("check_trials fails with non-numbers",{
  expect_error(check_trials("one"))
  expect_error(check_trials("probability"))
  expect_error(check_trials(TRUE))
})

test_that("test_success fails if trials is invalid",{
  expect_error(check_success(1, "one"))
  expect_error(check_success(1, -1))
  expect_error(check_success(1, 1.5))
  expect_error(check_success(1, c(1,2)))
})

test_that("test_success fails if success is larger than trials or if success is less than 0", {
  expect_error(check_success(4, 3))
  expect_error(check_success(-1, 3))
  expect_error(check_success(100, 99))
})

test_that("test_success fails if success not an integer", {
  expect_error(check_success(1.5, 3))
  expect_error(check_success((2/3), 3))
})

test_that("test_success works if success is an integer between trials and 0", {
  expect_true(check_success(2, 3))
  expect_true(check_success(3, 3))
  expect_true(check_success(0, 3))
})

test_that("test_success works even if success has multiple elements", {
  expect_true(check_success(c(1,2), 3))
  expect_true(check_success(1:5, 5))
  expect_true(check_success(c(1), 3))
})

test_that("test_success fails if success is non-number", {
  expect_error(check_success("one", 3))
  expect_error(check_success("probability", 3))
  expect_error(check_success(TRUE, 3))
})






