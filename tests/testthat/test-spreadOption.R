library(testthat)
library(RTL)

kirk_cases <- list(
  list(F1 = 92, F2 = 100, X = 5, sigma1 = 0.30, sigma2 = 0.35, rho = 0.85, T2M = 0.5, r = 0, call = 6.770610, put = 3.770610),
  list(F1 = 70, F2 = 80, X = 12, sigma1 = 0.45, sigma2 = 0.40, rho = 0.60, T2M = 1.0, r = 0, call = 10.310964, put = 12.310964),
  list(F1 = 100, F2 = 110, X = 5, sigma1 = 0.20, sigma2 = 0.25, rho = 0.50, T2M = 1.0, r = 0.05, call = 11.779001, put = 7.022854)
)

price <- function(case, type = "call", ...) {
  args <- utils::modifyList(case[c("F1", "F2", "X", "sigma1", "sigma2", "rho", "T2M", "r")], list(...))
  do.call(spreadOption, c(args, type = type))
}

test_that("spreadOption prices match an independent Kirk (1995) implementation to 6 decimals", {
  for (case in kirk_cases) {
    expect_equal(round(price(case, "call")$price, 6), case$call)
    expect_equal(round(price(case, "put")$price, 6), case$put)
  }
})

test_that("spreadOption satisfies put-call parity", {
  for (case in kirk_cases) {
    forward <- exp(-case$r * case$T2M) * (case$F2 - case$F1 - case$X)
    expect_equal(price(case, "call")$price - price(case, "put")$price, forward, tolerance = 1e-12)
  }
})

test_that("spreadOption Greeks are the derivatives of its price", {
  # Central differences of the returned price. With steps of 1e-4 (first order) and 1e-3 (second
  # order) the truncation and rounding error stays below 1e-6 relative for first derivatives and
  # 1e-5 for second derivatives on these cases.
  first <- function(case, type, arg, h = 1e-4) {
    bump <- function(v) do.call(price, c(list(case, type), stats::setNames(list(v), arg)))$price
    (bump(case[[arg]] + h) - bump(case[[arg]] - h)) / (2 * h)
  }
  second <- function(case, type, arg, h = 1e-3) {
    bump <- function(v) do.call(price, c(list(case, type), stats::setNames(list(v), arg)))$price
    (bump(case[[arg]] + h) - 2 * bump(case[[arg]]) + bump(case[[arg]] - h)) / h^2
  }
  cross <- function(case, type, h = 1e-3) {
    bump <- function(a, b) price(case, type, F1 = case$F1 + a, F2 = case$F2 + b)$price
    (bump(h, h) - bump(h, -h) - bump(-h, h) + bump(-h, -h)) / (4 * h^2)
  }
  for (case in kirk_cases) for (type in c("call", "put")) {
    g <- price(case, type)
    expect_equal(g$delta_F1, first(case, type, "F1"), tolerance = 1e-6)
    expect_equal(g$delta_F2, first(case, type, "F2"), tolerance = 1e-6)
    expect_equal(g$gamma_F1, second(case, type, "F1"), tolerance = 1e-5)
    expect_equal(g$gamma_F2, second(case, type, "F2"), tolerance = 1e-5)
    expect_equal(g$gamma_cross, cross(case, type), tolerance = 1e-5)
    expect_equal(g$vega_1, first(case, type, "sigma1"), tolerance = 1e-6)
    expect_equal(g$vega_2, first(case, type, "sigma2"), tolerance = 1e-6)
    expect_equal(g$theta, -first(case, type, "T2M"), tolerance = 1e-6)
    expect_equal(g$rho, first(case, type, "r"), tolerance = 1e-6)
  }
})

test_that("spreadOption rejects inputs outside Kirk's approximation", {
  case <- kirk_cases[[1]]
  expect_error(price(case, F2 = 0), "F2 > 0")
  expect_error(price(case, F1 = -10, X = 5), "F1 \\+ X > 0")
  expect_error(price(case, T2M = 0), "T2M must be positive")
  expect_error(price(case, rho = 1.5), "between -1 and 1")
  expect_error(price(case, sigma1 = 0, sigma2 = 0), "spread volatility is zero")
  expect_error(price(case, type = "straddle"), "'call' or 'put'")
})
