# to run tests, run:
# testthat::test_file("tests/testthat/test-impute-inf.R")
vec <- 1:5
test_1 <- c(Inf, 2, 3, 4, 5)
test_2 <- c(1, Inf, 3, 4, 5)
test_3 <- c(1, 2, Inf, 4, 5)
test_4 <- c(1, 2, 3, Inf, 5)
test_5 <- c(1, 2, 3, 4, Inf)

test_that("impute_inf imputes the right value", {
  expect_equal(impute_inf(vec, 1), test_1)
  expect_equal(impute_inf(vec, 2), test_2)
  expect_equal(impute_inf(vec, 3), test_3)
  expect_equal(impute_inf(vec, 4), test_4)
  expect_equal(impute_inf(vec, 5), test_5)
})

test_that("impute_inf doesn't work for vectors", {
  expect_error(impute_inf(vec, 1:2), test_1)
})
