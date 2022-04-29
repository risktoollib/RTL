#' OU process simulation
#' @description Simulates a Ornstein–Uhlenbeck process with mu as a function of time
#' @param S0 S at t=0
#' @param mu data frame of mean reversion level as a function of time
#' @param theta Mean reversion speed
#' @param sigma Standard deviation
#' @param T2M Maturity in years
#' @param dt Time step size e.g. 1/250 = 1 business day.
#' @return A numeric vector of simulated values
#' @export simOUt
#' @author Philippe Cote
#' @examples
#' mu = dplyr::tibble(t = 0:20,mr = c(rep(2,7),rep(4,14)))
#' simOUt(S0 = 5, mu = mu, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12)
simOUt <- function(S0 = 0, mu =  dplyr::tibble(t = 0:20,mr = c(rep(2,7),rep(4,14))), theta = 12, sigma = 0.2, T2M = 1, dt = 1/12)
{
  periods <- T2M/dt
  m <- dplyr::tibble(t = seq(0,T2M,dt),
                     mu.t = stats::approx(x = mu$t, y = mu$mr,xout = t)$y,
                     s = c(S0,rep(0,periods)))

  for (i in 2:(periods + 1)) {
    m[i,"s"] <- m[i - 1,"s"] + theta * (m[i,"mu.t"] - m[i - 1,"s"]) * dt + sigma *
      stats::rnorm(n = 1, mean = 0, sd = sqrt(dt))
  }
  return(m$s)
}
