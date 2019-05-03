context("check main functions")

# bin_choose

test_that("bin_choose computes right value", {
  expect_equal(bin_choose(5, 2), 10)
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
})

test_that("bin_choose computes right length", {
  expect_length(bin_choose(7, 4), 1)
  expect_length(bin_choose(7, 0:7), 8)
})

test_that("bin_choose fails when choosing invalid k", {
  expect_error(bin_choose(5, 6))
  expect_error(bin_choose(5, 2.5))
  expect_error(bin_choose(5, -1))
})

test_that("bin_choose fails when choosing invalid n", {
  expect_error(bin_choose(2.5, 1))
  expect_error(bin_choose(-2, 1))
  expect_error(bin_choose(3:4, 2))
})

# bin_probability

test_that("bin_probability computes right value", {
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_equal(bin_probability(0:2, 5, 0.5), c(0.03125, 0.15625, 0.31250))
})

test_that("bin_probability computes right length", {
  expect_length(bin_probability(4, 7, 0.5), 1)
  expect_length(bin_probability(0:7, 7, 0.5), 8)
})

test_that("bin_probability computes between 0 and 1", {
  expect_lte(bin_probability(5, 6, 0.5), 1)
  expect_gte(bin_probability(4, 5, 0.5), 0)
})

# bin_distribution

test_that("bin_distribution outputs class bindis", {
  expect_equal(class(bin_distribution()), c("bindis", "data.frame"))
})

test_that("bin_distribution outputs currect size", {
  expect_equal(nrow(bin_distribution()), 2)
  expect_equal(ncol(bin_distribution()), 2)
})

test_that("bin_distribution outputs dataframe", {
  expect_equal(typeof(bin_distribution()), "list")
})

# bin_cumulative

test_that("bin_cumulative outputs class bincum", {
  expect_equal(class(bin_cumulative()), c("bincum", "data.frame"))
})

test_that("bin_distribution outputs currect size", {
  expect_equal(nrow(bin_cumulative()), 2)
  expect_equal(ncol(bin_cumulative()), 3)
})

test_that("bin_distribution outputs dataframe", {
  expect_equal(typeof(bin_cumulative()), "list")
})
