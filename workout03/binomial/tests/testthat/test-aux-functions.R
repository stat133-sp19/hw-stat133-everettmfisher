context("check summary measures")

# aux_mean

test_that("aux_mean computes correct mean", {
  expect_equal(aux_mean(2, 0.5), 1)
})

test_that("aux_mean outputs correct length", {
  expect_length(aux_mean(2, 0.5), 1)
})

test_that("aux_mean outputs correct type", {
  expect_type(aux_mean(2, 0.5), "double")
})

# aux_variance

test_that("aux_variance computes correct variance", {
  expect_equal(aux_variance(2, 0.5), 0.5)
})

test_that("aux_variance outputs correct length", {
  expect_length(aux_variance(2, 0.5), 1)
})

test_that("aux_variance outputs correct type", {
  expect_type(aux_variance(2, 0.5), "double")
})

# aux_mode

test_that("aux_mode computes correct mode", {
  expect_equal(aux_mode(2, 0.5), 1)
  expect_equal(aux_mode(1, 0.5), c(1,0))
})

test_that("aux_mode outputs correct length", {
  expect_length(aux_mode(2, 0.5), 1)
  expect_length(aux_mode(1, 0.5), 2)
})

test_that("aux_mode outputs correct type", {
  expect_type(aux_mean(2, 0.5), "double")
})

# aux_skewness

test_that("aux_skewness computes correct value", {
  expect_equal(aux_skewness(2, 0.5), 0)
  expect_equal(abs(aux_skewness(2, 1)), Inf)
})

test_that("aux_skewness outputs correct length", {
  expect_length(aux_skewness(2, 0.5), 1)
})

test_that("aux_skewness outputs correct type", {
  expect_type(aux_skewness(2, 0.5), "double")
})

# aux_kurtosis

test_that("aux_kurtosis computes correct value", {
  expect_equal(aux_kurtosis(2, 0.5), -1)
  expect_equal(abs(aux_kurtosis(2, 1)), Inf)
})


test_that("aux_kurtosis outputs correct length", {
  expect_length(aux_kurtosis(2, 0.5), 1)
})

test_that("aux_kurtosis outputs correct type", {
  expect_type(aux_kurtosis(2, 0.5), "double")
})
