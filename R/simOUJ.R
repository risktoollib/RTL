#' OUJ process simulation
#' @description Simulates the mean-reverting jump-diffusion of Clewlow & Strickland (2000), eq. 2.17:
#' \deqn{dS = \theta(\ln \mu - \ln S) S dt + \sigma S dz + \kappa S dq}
#' The log price reverts to the log of the long-term level `mu` at speed `theta` (eq. 2.1). Jumps arrive `jump_prob`
#' times a year on average; each multiplies S by \eqn{1 + \kappa}, with
#' \eqn{\ln(1 + \kappa) \sim N(\ln(1 + \bar{\kappa}) - \gamma^2/2, \gamma^2)} (eq. 2.15), drawn independently for every
#' jump. The path is stepped in \eqn{x = \ln S} (eq. 2.3) plus the log jumps, so S stays positive.
#' @param nsims number of simulations. Defaults to 2. `numeric`
#' @param S0 S at t=0. `numeric`
#' @param mu Long-term level of S, \eqn{\bar{S} = e^{\mu}} in Clewlow & Strickland. `numeric`
#' @param theta Mean reversion speed of ln S, \eqn{\alpha} in Clewlow & Strickland. `numeric`
#' @param sigma Volatility of S, proportional (0.2 = 20% a year). `numeric`
#' @param jump_prob Average number of jumps per year, \eqn{\phi}. `numeric`
#' @param jump_avesize Mean proportional jump size \eqn{\bar{\kappa}} (0.4 = +40%). `numeric`
#' @param jump_stdv Jump volatility \eqn{\gamma}: the standard deviation of \eqn{\ln(1 + \kappa)}. `numeric`
#' @param T2M Maturity in years. `numeric`
#' @param dt Time step size e.g. 1/250 = 1 business day. `numeric`
#' @returns Simulated values. `tibble`
#' @references Clewlow, L. and Strickland, C. (2000). Energy Derivatives: Pricing and Risk Management. Lacima Publications. Eqs. 2.1-2.3, 2.15 and 2.17.
#' @export simOUJ
#' @author Philippe Cote
#' @examples
#' simOUJ(nsims = 2, S0 = 5, mu = 5, theta = .5, sigma = 0.2,
#' jump_prob = 0.05, jump_avesize = 0.6, jump_stdv = 0.05,
#' T2M = 1, dt = 1 / 12)
simOUJ <- function(nsims = 2, S0 = 5, mu = 5, theta = 10, sigma = 0.2, jump_prob = 0.05, jump_avesize = 0.4, jump_stdv = 0.05, T2M = 1, dt = 1 / 250) {
  periods <- round(T2M / dt)
  # Row 0 is ln S0; the other rows are the Brownian increments of each step.
  dz <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(dt)), ncol = nsims, nrow = periods)
  x <- rbind(rep(log(S0), nsims), dz)
  # The jumps of each step: a Poisson count n, each jump with its own ln(1 + kappa), so their sum is
  # N(n (ln(1 + jump_avesize) - jump_stdv^2 / 2), n jump_stdv^2).
  n <- matrix(stats::rpois(periods * nsims, jump_prob * dt), ncol = nsims, nrow = periods)
  djump <- n * (log(1 + jump_avesize) - jump_stdv^2 / 2) + jump_stdv * sqrt(n) * stats::rnorm(periods * nsims)
  djump <- rbind(rep(0, nsims), djump)
  # c++ implementation via ./src/rcppOUJ.cpp
  S <- exp(rcppOUJ(x, djump, theta, log(mu), dt, sigma))
  S <- dplyr::as_tibble(S, .name_repair = "minimal")
  names(S) <- paste0("sim",1:nsims)
  S <- S %>% dplyr::mutate(t = seq(0,T2M,dt)) %>% dplyr::select(t, dplyr::everything())

  # Check visual of diffusion
  # S %>% tidyr::pivot_longer(-t,"sim","value") %>% ggplot2::ggplot(ggplot2::aes(t,value,col = sim)) + ggplot2::geom_line() + theme(legend.position = "none")
  return(S)
}
