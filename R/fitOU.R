#' Fits a Ornstein–Uhlenbeck process to a dataset
#' @description Parameter estimation for Ornstein–Uhlenbeck process
#' @param spread Spread time series.
#' @return List of alpha, mu and sigma estimates
#' @export fitOU
#' @author Philippe Cote
#' @examples
#' spread <- simOU(mu = 5, theta = .5, sigma = 0.2, T = 5, dt = 1 / 250)
#' fitOU(spread)
fitOU <- function(spread) {
  n <- length(spread)
  delta <- 1 # delta
  Sx <- sum(spread[1:n - 1])
  Sy <- sum(spread[2:n])
  Sxx <- sum((spread[1:n - 1])^2)
  Syy <- sum((spread[2:n])^2)
  Sxy <- sum((spread[1:n - 1]) * (spread[2:n]))
  mu <- (Sy * Sxx - Sx * Sxy) / ((n - 1) * (Sxx - Sxy) - (Sx^2 - Sx * Sy))
  theta <- -log((Sxy - mu * Sx - mu * Sy + (n - 1) * mu^2) / (Sxx - 2 * mu *
    Sx + (n - 1) * mu^2)) / delta
  a <- exp(-theta * delta)
  sigmah2 <- (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * mu * (1 - a) * (Sy - a * Sx) + (n - 1) * mu^2 * (1 - a)^2) / (n - 1)
  sigma <- sqrt((sigmah2) * 2 * theta / (1 - a^2))
  theta <- list(theta = theta, mu = mu, sigma = sigma)
  return(theta)
}
