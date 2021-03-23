#' \code{simGBM}
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
#' simGBM(S0=10,drift=0,sigma=0.2,T2M=1,dt=1/12)

simGBM <- function(S0=10,drift=0,sigma=0.2,T2M=1,dt=1/12) {
  periods = T2M/dt
  S = rep(S0,periods)
  for (i in 2:periods) {
    S[i] = S[i-1] * exp(
      (drift - (sigma^2)/2) * dt +
        sigma * stats::rnorm(1,mean=0,sd=1) * sqrt(dt)
    )
  }
  return(S)
}

# library(profvis)
# library(tidyverse)
# T2M = 10
# dt = 1/365
# profvis({
#   m <- replicate(2000, simGBM(S0 = S0,drift = drift,sigma = sigma,T = T,dt = dt))
#   as_tibble(m)
#   })
