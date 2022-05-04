#' GBM process simulation
#' @description Simulates a Geometric Brownian Motion process
#' @param nsims numble of simulations. Defaults to 1
#' @param S0 Spot price at t=0
#' @param drift Drift term in percentage
#' @param sigma Standard deviation
#' @param T2M Maturity in years
#' @param dt Time step in period e.g. 1/250 = 1 business day.
#' @param vec Vectorized implementation. Defaults to TRUE
#' @return A tibble of simulated values
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
    S <- dplyr::as_tibble(S,.name_repair = "universal")
    names(S) <- paste("sim",1:nsims,sep = "")
  }
  return(S)
}
