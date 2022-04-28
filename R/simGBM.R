#' GBM process simulation
#' @description Simulates a Geometric Brownian Motion process
#' @param S0 Spot price at t=0
#' @param drift Drift term in percentage
#' @param sigma Standard deviation
#' @param T2M Maturity in years
#' @param dt Time step in period e.g. 1/250 = 1 business day.
#' @return A numeric vector of simulated values
#' @export simGBM
#' @author Philippe Cote
#' @examples
#' simGBM(S0 = 10, drift = 0, sigma = 0.2, T2M = 1, dt = 1 / 12)
simGBM <- function(S0 = 10, drift = 0, sigma = 0.2, T2M = 1, dt = 1 / 12) {
  periods <- T2M / dt
  S <- rep(S0, periods)
  for (i in 2:periods) {
    S[i] <- S[i - 1] * exp(
      (drift - (sigma^2) / 2) * dt +
        sigma * stats::rnorm(1, mean = 0, sd = 1) * sqrt(dt)
    )
  }
  return(S)
}


# simOUt <- function (S0 = 0, mu = dplyr::tibble(t = 0:20,mr = c(rep(0,7),rep(2,14))), theta = 12, sigma = 0.2, T2M = 1, dt = 1/12) 
# {
#   periods <- T2M/dt
#   m <- dplyr::tibble(t = seq(0,T2M,dt),
#                      mu.t = stats::approx(x = muT$t, y = muT$mr,xout = t)$y,
#                      s = c(S0,rep(0,periods)))
# 
#   for (i in 2:(periods + 1)) {
#     m[i,"s"] <- m[i - 1,"s"] + theta * (m[i,"mu.t"] - m[i - 1,"s"]) * dt + sigma * 
#       stats::rnorm(n = 1, mean = 0, sd = sqrt(dt))
#   }
#   return(m$s)
# }
