#' Generalized Black-Scholes (GBS) Option Pricing Model
#'
#' Computes the price and Greeks of European call and put options using the
#' Generalized Black-Scholes model.
#'
#' @param S numeric, the current stock price (also known as the underlying asset price).
#' @param X numeric, the strike price of the option.
#' @param T2M numeric, the time to maturity (in years). Previously denoted as T.
#' @param r numeric, the risk-free interest rate (annualized).
#' @param b numeric, the cost of carry, b = r - q for dividend paying assets,
#' where q is the dividend yield rate.
#' @param sigma numeric, the volatility of the underlying asset (annualized).
#' @param type character, the type of option to evaluate, either "call" or "put".
#' Default is "call".
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \code{price}: The price of the option.
#'   \item \code{delta}: The sensitivity of the option's price to a change in the
#'   price of the underlying asset.
#'   \item \code{gamma}: The rate of change in the delta with respect to changes
#'   in the underlying price.
#'   \item \code{vega}: The sensitivity of the option's price to the volatility of
#'   the underlying asset.
#'   \item \code{theta}: The sensitivity of the option's price to the passage of time.
#'   \item \code{rho}: The sensitivity of the option's price to the interest rate.
#' }
#'
#' @examples
#' GBSOption(S = 100, X = 100, T2M = 1, r = 0.05, b = 0.02, sigma = 0.2, type = "call")
#'
#' @export GBSOption
GBSOption <- function(S, X, T2M, r, b, sigma, type = "call") {
  if (sigma == 0) {sigma <- 10^-16}
  d1 <- (log(S/X) + (b + sigma^2 / 2) * T2M) / (sigma * sqrt(T2M))
  d2 <- d1 - sigma * sqrt(T2M)

  if (type == "call") {
    price <- S * exp((b - r) * T2M) * pnorm(d1) - X * exp(-r * T2M) * pnorm(d2)
    delta <- exp((b - r) * T2M) * pnorm(d1)
    gamma <- exp((b - r) * T2M) * dnorm(d1) / (S * sigma * sqrt(T2M))
    vega <- S * sqrt(T2M) * exp((b - r) * T2M) * dnorm(d1)
    theta <- - (S * dnorm(d1) * sigma * exp((b - r) * T2M)) / (2 * sqrt(T2M)) - r * X * exp(-r * T2M) * pnorm(d2) + (b - r) * S * exp((b - r) * T2M) * pnorm(d1)
    rho <- X * T2M * exp(-r * T2M) * pnorm(d2)
  } else if (type == "put") {
    price <- X * exp(-r * T2M) * pnorm(-d2) - S * exp((b - r) * T2M) * pnorm(-d1)
    delta <- -exp((b - r) * T2M) * pnorm(-d1)
    gamma <- exp((b - r) * T2M) * dnorm(d1) / (S * sigma * sqrt(T2M))
    vega <- S * sqrt(T2M) * exp((b - r) * T2M) * dnorm(d1)
    theta <- - (S * dnorm(d1) * sigma * exp((b - r) * T2M)) / (2 * sqrt(T2M)) + r * X * exp(-r * T2M) * pnorm(-d2) - (b - r) * S * exp((b - r) * T2M) * pnorm(-d1)
    rho <- -X * T2M * exp(-r * T2M) * pnorm(-d2)
  } else {
    stop("Invalid option type")
  }

  return(list(price = price, delta = delta, gamma = gamma, vega = vega, theta = theta, rho = rho))
}
