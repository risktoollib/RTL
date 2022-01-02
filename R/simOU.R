#' OU process simulation
#' @description Simulates a Ornstein–Uhlenbeck process
#' @param S0 S at t=0
#' @param mu Mean reversion level
#' @param theta Mean reversion speed
#' @param sigma Standard deviation
#' @param T2M Maturity in years
#' @param dt Time step size e.g. 1/250 = 1 business day.
#' @return A numeric vector of simulated values
#' @export simOU
#' @author Philippe Cote
#' @examples
#' simOU(S0 = 5, mu = 5, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12)
simOU <- function(S0 = 5, mu = 5, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12) {
  periods <- T2M / dt
  S <- rep(S0, periods)
  for (i in 2:periods) {
    S[i] <- S[i - 1] + theta * (mu - S[i - 1]) * dt + sigma * stats::rnorm(n = 1, mean = 0, sd = sqrt(dt))
  }
  return(S)
}
