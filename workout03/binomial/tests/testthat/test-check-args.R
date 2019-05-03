context("check binomial arguments")

# check_prob

test_that("check_prob with ok values", {
  expect_true(check_prob(0))
  expect_true(check_prob(1))
  expect_true(check_prob(0.5))
})

test_that("check_prob fails with invalid lengths", {
  expect_error(check_prob(c(0.2, 0.3)))
})

test_that("check_prob fails with invalid types", {
  expect_error(check_prob("0.3"))
  expect_error(check_prob(list(0.2)))
  expect_error(check_prob(TRUE))
})

test_that("check_prob fails with invalid range", {
  expect_error(check_prob(-0.1))
  expect_error(check_prob(1.1))
})

# check_trials

test_that("check_trials with ok values", {
  expect_true(check_trials(0))
  expect_true(check_trials(1))
  expect_true(check_trials(20))
})

test_that("check_trials fails with invalid lengths", {
  expect_error(check_trials(c(10, 15)))
})

test_that("check_trials fails with invalid types", {
  expect_error(check_trials("5"))
  expect_error(check_trials(list(8)))
  expect_error(check_trials(TRUE))
})

test_that("check_trials fails with invalid range", {
  expect_error(check_trials(-2))
})

test_that("check_trials fails with non-integer", {
  expect_error(check_trials(5.5))
})

# check_success

test_that("check_success with ok values", {
  expect_true(check_success(0, 5))
  expect_true(check_success(5, 5))
  expect_true(check_success(c(7, 10), 12))
})

test_that("check_success fails with invalid types", {
  expect_error(check_success("5", 8))
  expect_error(check_success(list(8), 10))
  expect_error(check_success(TRUE, 2))
})

test_that("check_success fails with invalid range", {
  expect_error(check_success(-2, 10))
  expect_error(check_success(1:10, 8))
})

test_that("check_trials fails with non-integer", {
  expect_error(check_success(5.5, 6))
})
