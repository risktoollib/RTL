#' \code{simOUJ}
#' @description Simulates a Ornstein–Uhlenbeck process with Jumps
#' @param S0 S at t=0
#' @param mu Mean reversion level
#' @param theta Mean reversion speed
#' @param sigma Standard deviation
#' @param jump_prob Probability of jumps
#' @param jump_avesize Average size of jumps
#' @param jump_stdv Standard deviation of jump average size
#' @param T2M Maturity in years
#' @param dt Time step size e.g. 1/250 = 1 business day.
#' @return A numeric vector of simulated values
#' @export simOUJ
#' @author Philippe Cote
#' @examples
#' simOUJ(S0=5,mu=5,theta=.5,sigma=0.2,jump_prob=0.05,jump_avesize = 3,jump_stdv = 0.05,T2M=1,dt=1/12)

simOUJ <- function(S0=5,mu=5,theta=10,sigma=0.2,jump_prob=0.05,jump_avesize = 2,jump_stdv = 0.05,T2M=1,dt=1/250) {
  periods = T2M/dt
  #JperYear = jump_prob / dt
  S = rep(S0, periods)
  for (i in 2:periods) {
    S[i] = S[i - 1] +
      # theta * (log(mu) - (JperYear * jump_avesize) - log(S[i-1])) * S[i-1] * dt +
      # theta * (log(mu)  - (jump_prob * jump_avesize) - log(S[i-1])) * S[i-1] * dt + # Clewlow
      theta * (mu  - (jump_prob * jump_avesize) - S[i-1]) * S[i-1] * dt +
      sigma * S[i-1] * stats::rnorm(n = 1, mean = 0, sd = sqrt(dt)) +
      stats::rpois(1, jump_prob * dt) * stats::rlnorm(n = 1, mean = log(jump_avesize), sd = jump_stdv)
  }
  # periods = T2M/dt
  # S0 = mu
  # S = rep(S0,periods)
  # for (i in 2:periods) {
  #   S[i] = S[i-1] + theta * (mu + sigma^2/(2*theta) - log(S[i-1])) * dt + sigma * rnorm(n=1,mean=0,sd=sqrt(dt)) + rpois(1, jump_prob*dt) * rnorm(n=1,mean=jump_avesize,sd=jump_stdv)
  #   }
  return(S)
}
