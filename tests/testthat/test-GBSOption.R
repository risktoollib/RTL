library(testthat)
library(RTL)

gbs_cases <- list(
  list(S = 100, X = 95, T2M = 0.5, r = 0.05, b = 0.02, sigma = 0.30, call = 11.335578, put = 5.478826, theta_call = -8.432097, theta_put = -6.754710),
  list(S = 60, X = 75, T2M = 1.25, r = 0.03, b = -0.01, sigma = 0.45, call = 6.605664, put = 21.771480, theta_call = -4.041202, theta_put = -4.156965),
  list(S = 100, X = 95, T2M = 0.5, r = 0.05, b = 0, sigma = 0.30, call = 10.703499, put = 5.826950, theta_call = -7.234126, theta_put = -7.477954)
)

gbs <- function(case, type = "call", ...) {
  args <- utils::modifyList(case[c("S", "X", "T2M", "r", "b", "sigma")], list(...))
  do.call(GBSOption, c(args, type = type))
}

test_that("GBSOption prices and thetas match an independent implementation to 6 decimals", {
  for (case in gbs_cases) {
    expect_equal(round(gbs(case, "call")$price, 6), case$call)
    expect_equal(round(gbs(case, "put")$price, 6), case$put)
    expect_equal(round(gbs(case, "call")$theta, 6), case$theta_call)
    expect_equal(round(gbs(case, "put")$theta, 6), case$theta_put)
  }
})

test_that("GBSOption Greeks are the derivatives of its price", {
  # Central differences of the returned price. rho moves b with r (q = r - b fixed) unless b = 0, an option on a
  # futures price, where b stays zero.
  first <- function(case, type, arg, h = 1e-4) {
    bump <- function(v) do.call(gbs, c(list(case, type), stats::setNames(list(v), arg)))$price
    (bump(case[[arg]] + h) - bump(case[[arg]] - h)) / (2 * h)
  }
  second <- function(case, type, arg, h = 1e-3) {
    bump <- function(v) do.call(gbs, c(list(case, type), stats::setNames(list(v), arg)))$price
    (bump(case[[arg]] + h) - 2 * bump(case[[arg]]) + bump(case[[arg]] - h)) / h^2
  }
  for (case in gbs_cases) for (type in c("call", "put")) {
    g <- gbs(case, type)
    expect_equal(g$delta, first(case, type, "S"), tolerance = 1e-6)
    expect_equal(g$gamma, second(case, type, "S"), tolerance = 1e-5)
    expect_equal(g$vega, first(case, type, "sigma"), tolerance = 1e-6)
    expect_equal(g$theta, -first(case, type, "T2M"), tolerance = 1e-6)
    rate <- function(h) gbs(case, type, r = case$r + h, b = if (case$b == 0) 0 else case$b + h)$price
    expect_equal(g$rho, (rate(1e-4) - rate(-1e-4)) / 2e-4, tolerance = 1e-6)
  }
})

test_that("GBSOption theta and rho match RTL's own binomial tree", {
  # CRROption (C++) prices the same European option without using GBSOption. Its theta (-dV/dT) and rho (dV/dr)
  # by central difference, averaging N and N + 1 steps to damp the binomial oscillation, are within 0.02 of the
  # closed form at N = 4000 (the gap shrinks about fourfold for fourfold N). The 1.3.8 theta missed by 1.9 to 6.2,
  # and its rho at b = 0 by 31.
  crr <- function(case, type, N, ...) {
    a <- utils::modifyList(case, list(...))
    (CRROption(a$S, a$X, a$sigma, a$r, a$b, a$T2M, N, type, "european")$price +
       CRROption(a$S, a$X, a$sigma, a$r, a$b, a$T2M, N + 1, type, "european")$price) / 2
  }
  h <- 1e-3
  for (case in gbs_cases) for (type in c("call", "put")) {
    g <- gbs(case, type)
    theta <- -(crr(case, type, 4000, T2M = case$T2M + h) - crr(case, type, 4000, T2M = case$T2M - h)) / (2 * h)
    db <- if (case$b == 0) 0 else h  # b moves with r unless the option is on a futures price
    rho <- (crr(case, type, 4000, r = case$r + h, b = case$b + db) - crr(case, type, 4000, r = case$r - h, b = case$b - db)) / (2 * h)
    expect_lt(abs(g$theta - theta), 0.02)
    expect_lt(abs(g$rho - rho), 0.02)
  }
})
