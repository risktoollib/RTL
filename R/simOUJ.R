#' OUJ process simulation
#' @description Simulates a Ornstein–Uhlenbeck process with Jumps
#' @param nsims number of simulations. Defaults to 2. `numeric`
#' @param S0 S at t=0. `numeric`
#' @param mu Mean reversion level. `numeric`
#' @param theta Mean reversion speed. `numeric`
#' @param sigma Standard deviation. `numeric`
#' @param jump_prob Probability of jumps. `numeric`
#' @param jump_avesize Average size of jumps. `numeric`
#' @param jump_stdv Standard deviation of jump average size. `numeric`
#' @param T2M Maturity in years. `numeric`
#' @param dt Time step size e.g. 1/250 = 1 business day. `numeric`
#' @returns Simulated values. `tibble`
#' @export simOUJ
#' @author Philippe Cote
#' @examples
#' simOUJ(nsims = 2, S0 = 5, mu = 5, theta = .5, sigma = 0.2,
#' jump_prob = 0.05, jump_avesize = 3, jump_stdv = 0.05,
#' T2M = 1, dt = 1 / 12)
simOUJ <- function(nsims = 2, S0 = 5, mu = 5, theta = 10, sigma = 0.2, jump_prob = 0.05, jump_avesize = 2, jump_stdv = 0.05, T2M = 1, dt = 1 / 250) {

  periods <- T2M / dt
  dz <- NULL
  dz <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(dt)),
                      ncol = nsims,
                      nrow = periods)
  dz <- rbind(rep(S0,nsims),dz)
  djump <- NULL
  djump <- matrix(stats::rpois(periods * nsims, jump_prob * dt) * stats::rlnorm(n = 1, mean = log(jump_avesize), sd = jump_stdv),
                  ncol = nsims,
                  nrow = periods)
  djump <- rbind(rep(0,nsims),djump)

  # c++ implementation via ./src/rcppOUJ.cpp
  # rcppOUJ <- NULL
  # Rcpp::cppFunction("
  # NumericMatrix rcppOUJ(NumericMatrix x, NumericMatrix djump, double theta, double mu, double dt, double sigma, double jump_prob, double jump_avesize) {
  #   for (int i = 1; i < x.nrow(); i++) {
  #     for (int j = 0; j < x.ncol(); j++) {
  #      x(i,j) =  x(i-1,j) + theta * (mu - (jump_prob * jump_avesize) - x(i-1,j)) * dt + sigma * x(i,j) + djump(i,j);
  #     }
  #   }
  #   return x;
  # }
  #                   ")

  # R version
  # periods <- T2M / dt
  # # JperYear = jump_prob / dt
  # S <- rep(S0, periods)
  # for (i in 2:periods) {
  #   S[i] <- S[i - 1] +
  #     # theta * (log(mu)  - (jump_prob * jump_avesize) - log(S[i-1])) * S[i-1] * dt + # Clewlow
  #     theta * (mu - (jump_prob * jump_avesize) - S[i - 1]) * S[i - 1] * dt +
  #     sigma * S[i - 1] * stats::rnorm(n = 1, mean = 0, sd = sqrt(dt)) +
  #     stats::rpois(1, jump_prob * dt) * stats::rlnorm(n = 1, mean = log(jump_avesize), sd = jump_stdv)
  # }

  S <- rcppOUJ(dz,djump,theta,mu,dt,sigma,jump_prob,jump_avesize)
  S <- dplyr::as_tibble(S, .name_repair = "minimal")
  names(S) <- paste0("sim",1:nsims)
  S <- S %>% dplyr::mutate(t = seq(0,T2M,dt)) %>% dplyr::select(t, dplyr::everything())

  # Check visual of diffusion
  # S %>% tidyr::pivot_longer(-t,"sim","value") %>% ggplot2::ggplot(ggplot2::aes(t,value,col = sim)) + ggplot2::geom_line() + theme(legend.position = "none")
  return(S)
}
