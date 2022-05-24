#' OU process simulation
#' @description Simulates a Ornstein–Uhlenbeck process with mu as a function of time
#' @param nsims number of simulations. Defaults to 2
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
#' simOUt(nsims = 2, S0 = 5, mu = mu, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12)
simOUt <- function(nsims = 2, S0 = 0, mu =  dplyr::tibble(t = 0:20,mr = c(rep(2,7),rep(4,14))), theta = 12, sigma = 0.2, T2M = 1, dt = 1/12)
{
  periods <- T2M/dt
  m <- dplyr::tibble(t = seq(0,T2M,dt),
                     mu.t = stats::approx(x = mu$t, y = mu$mr,xout = t)$y,
                     s = c(S0,rep(0,periods)))
  ## cpp
  diffusion <- NULL
  diffusion <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(dt)), ncol = nsims, nrow = periods)
  diffusion <- rbind(rep(S0,nsims),diffusion)
  diffusion <- cbind(m$mu.t, diffusion)

  # c++ implementation via ./src/rcppOUt.cpp
    # rcppOUt <- NULL
    # Rcpp::cppFunction("
    # NumericMatrix rcppOUt(NumericMatrix x, double theta, double dt, double sigma) {
    #   for (int i = 1; i < x.nrow(); i++) {
    #     for (int j = 1; j < x.ncol(); j++) {
    #      x(i,j) =  x(i-1,j) + theta * (x(i,0) - x(i-1,j)) * dt + sigma * x(i,j) ;
    #     }
    #   }
    #   return x;
    # }
    #                  ")
  S <- rcppOUt(diffusion,theta,dt,sigma)
  S <- dplyr::as_tibble(S, .name_repair = "minimal")
  names(S) <- c("mu",paste0("sim",1:nsims))
  S <- S %>% dplyr::select(-mu) %>% dplyr::mutate(t = seq(0,T2M,dt)) %>% dplyr::select(t, dplyr::everything())

  # Check visual of diffusion
  # S %>% tidyr::pivot_longer(-t,"sim","value") %>% ggplot2::ggplot(ggplot2::aes(t,value,col = sim)) + ggplot2::geom_line() + theme(legend.position = "none")

  # R implementation
  # for (i in 2:(periods + 1)) {
  #   m[i,"s"] <- m[i - 1,"s"] + theta * (m[i,"mu.t"] - m[i - 1,"s"]) * dt + sigma *
  #     stats::rnorm(n = 1, mean = 0, sd = sqrt(dt))
  # }
  return(S)
}


nsims = 50
S0 = 0
mu =  dplyr::tibble(t = 0:20,mr = c(rep(2,7),rep(4,14)))
theta = 12
sigma = 0.2
T2M = 10
dt = 1/12
