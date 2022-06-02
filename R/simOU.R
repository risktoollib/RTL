#' OU process simulation
#' @description Simulates a Ornstein–Uhlenbeck process
#' @param nsims number of simulations. Defaults to 2
#' @param S0 S at t=0
#' @param mu Mean reversion level
#' @param theta Mean reversion speed
#' @param sigma Standard deviation
#' @param T2M Maturity in years
#' @param dt Time step size e.g. 1/250 = 1 business day.
#' @param epsilon Defaults to NULL function generates its own.
#' OPTIONAL: Array of epsilons for nsims = 1, if you want to feed your own e.g. in a multivariate context.
#' @return A numeric vector of simulated values
#' @export simOU
#' @author Philippe Cote
#' @examples
#' simOU(nsims = 5, S0 = 5, mu = 5, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12, epsilon = NULL)
#' simOU(nsims = 1, S0 = 5, mu = 5, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12,
#' epsilon = matrix(rnorm(12,0,sqrt(1/12))))
#' simOU(nsims = 2, S0 = 5, mu = 5, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12,
#' epsilon = replicate(2,rnorm(12,0,sqrt(1/12))))
simOU <- function(nsims = 2, S0 = 5, mu = 5, theta = .5, sigma = 0.2, T2M = 1, dt = 1 / 12, epsilon = NULL) {
  periods <- T2M / dt
  diffusion <- NULL
  if (is.null(epsilon)) {
    diffusion <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(dt)), ncol = nsims, nrow = periods)
  } else {
    if (nsims != dim(epsilon)[2]) {stop("nsims must correspond to number of columns in epsilon")}
    diffusion <- epsilon
    }

  diffusion <- rbind(rep(S0,nsims),diffusion)

  # c++ implementation via ./src/rcppOUt.cpp
    # rcppOU <- NULL
    # Rcpp::cppFunction("
    # NumericMatrix rcppOU(NumericMatrix x, double theta, double mu, double dt, double sigma) {
    #   for (int i = 1; i < x.nrow(); i++) {
    #     for (int j = 0; j < x.ncol(); j++) {
    #      x(i,j) =  x(i-1,j) + theta * (mu - x(i-1,j)) * dt + sigma * x(i,j) ;
    #     }
    #   }
    #   return x;
    # }
    #                   ")
  S <- rcppOU(diffusion,theta,mu,dt,sigma)
  S <- dplyr::as_tibble(S, .name_repair = "minimal")
  names(S) <- paste0("sim",1:nsims)
  S <- S %>% dplyr::mutate(t = seq(0,T2M,dt)) %>% dplyr::select(t, dplyr::everything())

  # Check visual of diffusion
  # S %>% tidyr::pivot_longer(-t,"sim","value") %>% ggplot2::ggplot(ggplot2::aes(t,value,col = sim)) + ggplot2::geom_line() + theme(legend.position = "none")

  ## R version
  # S <- rep(S0, periods)
  # for (i in 2:periods) {
  #   S[i] <- S[i - 1] + theta * (mu - S[i - 1]) * dt + sigma * stats::rnorm(n = 1, mean = 0, sd = sqrt(dt))
  # }
  return(S)
}
