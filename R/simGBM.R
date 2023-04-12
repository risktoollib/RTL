#' GBM process simulation
#' @description Simulates a Geometric Brownian Motion process
#' @param nsims number of simulations. Defaults to 1. `numeric`
#' @param S0 Spot price at t=0. `numeric`
#' @param drift Drift term in percentage. `numeric`
#' @param sigma Standard deviation. `numeric`
#' @param T2M Maturity in years. `numeric`
#' @param dt Time step in period e.g. 1/250 = 1 business day. `numeric`
#' @param vec Vectorized implementation. Defaults to TRUE. `logical`
#' @returns A tibble of simulated values. `tibble`
#' @export simGBM
#' @author Philippe Cote
#' @examples
#' simGBM(nsims = 2, S0 = 10, drift = 0, sigma = 0.2, T2M = 1, dt = 1 / 12, vec = TRUE)
simGBM <- function(nsims = 1, S0 = 10, drift = 0, sigma = 0.2, T2M = 1, dt = 1 / 12, vec = TRUE) {
  periods <- T2M / dt
  if (vec == FALSE & nsims > 1) stop("For nsmis > 1, use set vec to TRUE. Non-vectorize version only shown for education purposes.")
  if (vec == FALSE) {
    S <- rep(S0, periods + 1)
    for (i in 2:(periods + 1)) {
      S[i] <- S[i - 1] * exp((drift - (sigma^2) / 2) * dt + sigma * stats::rnorm(1, mean = 0, sd = sqrt(dt)))
    }
  } else {
    diffusion <- NULL
    diffusion <- matrix(stats::rnorm(periods * nsims, mean = 0, sd = sqrt(dt)), ncol = nsims, nrow = periods)
    S <- exp((drift - (sigma^2) / 2) * dt + sigma * diffusion)
    S <- apply(rbind(rep(S0,nsims),S),2,cumprod)
    S <- dplyr::as_tibble(S,.name_repair = "minimal")
    names(S) <- paste("sim",1:nsims,sep = "")
    S <- S %>% dplyr::mutate(t = seq(0,T2M,dt)) %>% dplyr::select(t, dplyr::everything())
  }
  return(S)
}
