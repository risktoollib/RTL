# simOUJ is Clewlow & Strickland (2000), eq. 2.17, stepped in x = ln S (eq. 2.3) plus log jumps (eq. 2.15).

test_that("without noise or jumps, ln S follows the eq. 2.3 recursion exactly", {
  S <- simOUJ(nsims = 1, S0 = 8, mu = 5, theta = 3, sigma = 0, jump_prob = 0, T2M = 1, dt = 1 / 250)
  i <- 0:250
  expected <- log(5) + (log(8) - log(5)) * (1 - 3 / 250)^i
  expect_equal(nrow(S), 251)
  expect_equal(log(S$sim1), expected, tolerance = 1e-12)
})

test_that("with gamma = 0 every jump multiplies S by exactly 1 + jump_avesize", {
  set.seed(1)
  S <- simOUJ(nsims = 20, S0 = 5, mu = 5, theta = 0, sigma = 0, jump_prob = 50, jump_avesize = 0.4,
              jump_stdv = 0, T2M = 1, dt = 1 / 250)
  dx <- diff(as.matrix(log(S[, -1])))
  jumps <- dx / log(1.4)
  expect_true(sum(dx != 0) > 100)
  expect_equal(jumps, round(jumps), tolerance = 1e-12)
})

test_that("each jump draws its own size", {
  set.seed(2)
  S <- simOUJ(nsims = 20, S0 = 5, mu = 5, theta = 0, sigma = 0, jump_prob = 50, jump_avesize = 0.4,
              jump_stdv = 0.3, T2M = 1, dt = 1 / 250)
  dx <- diff(as.matrix(log(S[, -1])))
  expect_true(length(unique(round(dx[dx != 0], 12))) > 100)
})

test_that("a jump raises S by jump_avesize on average: E[S_T / S0] = exp(jump_prob T jump_avesize) (eq. 2.15)", {
  set.seed(3)
  S <- simOUJ(nsims = 20000, S0 = 5, mu = 5, theta = 0, sigma = 0, jump_prob = 2, jump_avesize = 0.1,
              jump_stdv = 0.2, T2M = 1, dt = 1 / 50)
  ratio <- unlist(S[nrow(S), -1]) / 5
  se <- stats::sd(ratio) / sqrt(length(ratio))
  expect_lt(abs(mean(ratio) - exp(2 * 1 * 0.1)), 4 * se)
})

test_that("without jumps ln S reverts to ln(mu) - sigma^2 / (2 theta) (eq. 2.2)", {
  set.seed(4)
  S <- simOUJ(nsims = 20000, S0 = 5, mu = 5, theta = 4, sigma = 0.5, jump_prob = 0, T2M = 5, dt = 1 / 50)
  x <- log(unlist(S[nrow(S), -1]))
  se <- stats::sd(x) / sqrt(length(x))
  expect_lt(abs(mean(x) - (log(5) - 0.5^2 / (2 * 4))), 4 * se)
  expect_true(all(unlist(S[, -1]) > 0))
})
