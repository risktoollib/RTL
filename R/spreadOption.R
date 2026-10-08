#' Kirk's Approximation for Spread Option Pricing
#'
#' Computes the price and Greeks of European spread options using Kirk's 1995
#' approximation. The spread option gives the holder the right to receive the
#' difference between two asset prices (F2 - F1) at maturity, if positive,
#' in exchange for paying the strike price X.
#'
#' @param F1 numeric, the forward price of the first asset.
#' @param F2 numeric, the forward price of the second asset.
#' @param X numeric, the strike price of the spread option.
#' @param sigma1 numeric, the volatility of the first asset (annualized).
#' @param sigma2 numeric, the volatility of the second asset (annualized).
#' @param rho numeric, the correlation coefficient between the two assets (-1 <= rho <= 1).
#' @param T2M numeric, the time to maturity in years.
#' @param r numeric, the risk-free interest rate (annualized).
#' @param type character, the type of option to evaluate, either "call" or "put".
#'        Default is "call".
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \code{price}: The price of the spread option
#'   \item \code{delta_F1}: The sensitivity of the option price to changes in F1
#'   \item \code{delta_F2}: The sensitivity of the option price to changes in F2
#'   \item \code{gamma_F1}: The second derivative of the option price with respect to F1
#'   \item \code{gamma_F2}: The second derivative of the option price with respect to F2
#'   \item \code{gamma_cross}: The mixed second derivative with respect to F1 and F2
#'   \item \code{vega_1}: The sensitivity of the option price to changes in sigma1
#'   \item \code{vega_2}: The sensitivity of the option price to changes in sigma2
#'   \item \code{theta}: The sensitivity of the option price to the passage of time
#'   \item \code{rho}: The sensitivity of the option price to changes in the interest rate
#' }
#'
#' @details
#' Kirk (1995) treats \code{F1 + X} as one lognormal asset and prices the spread with Black's formula:
#' \deqn{c = e^{-rT}\left[F_2 N(d_1) - (F_1 + X) N(d_2)\right],\quad
#' d_1 = \frac{\ln(F_2/(F_1+X)) + \sigma^2 T/2}{\sigma\sqrt{T}},\quad d_2 = d_1 - \sigma\sqrt{T},}
#' with \eqn{\sigma^2 = \sigma_2^2 - 2\rho\sigma_1\sigma_2 w + \sigma_1^2 w^2} and \eqn{w = F_1/(F_1 + X)}.
#' The put follows from put-call parity, \eqn{p = c - e^{-rT}(F_2 - F_1 - X)}.
#'
#' Every Greek is the exact derivative of the returned price, including the dependence of
#' \eqn{\sigma} on \code{F1} through \eqn{w}: \code{delta_F1} and \code{gamma_F1} carry the terms
#' that a fixed-volatility Black delta omits. \code{theta} is \eqn{-\partial V/\partial T}
#' (per year) and \code{rho} is \eqn{\partial V/\partial r}.
#'
#' The approximation needs \code{F2 > 0} and \code{F1 + X > 0}; other inputs stop with an error.
#'
#' @references
#' Kirk, E. (1995) "Correlation in the Energy Markets." Managing Energy Price Risk,
#' Risk Publications and Enron, London, pp. 71-78.
#'
#' @examples
#' # Price a call spread option with the following parameters:
#' F1 <- 100  # Forward price of first asset
#' F2 <- 110  # Forward price of second asset
#' X <- 5     # Strike price
#' sigma1 <- 0.2  # Volatility of first asset
#' sigma2 <- 0.25 # Volatility of second asset
#' rho <- 0.5     # Correlation between assets
#' T2M <- 1       # One year to maturity
#' r <- 0.05      # Risk-free rate
#'
#' result_call <- spreadOption(F1, F2, X, sigma1, sigma2, rho, T2M, r, type = "call")
#' result_put <- spreadOption(F1, F2, X, sigma1, sigma2, rho, T2M, r, type = "put")
#'
#' @export spreadOption
spreadOption <- function(F1, F2, X, sigma1, sigma2, rho, T2M, r, type = "call") {
  if (!type %in% c("call", "put")) stop("Type must be 'call' or 'put'")
  if (F2 <= 0) stop("Kirk's approximation needs F2 > 0")
  if (F1 + X <= 0) stop("Kirk's approximation needs F1 + X > 0")
  if (T2M <= 0) stop("T2M must be positive")
  if (sigma1 < 0 || sigma2 < 0) stop("Volatilities must be non-negative")
  if (abs(rho) > 1) stop("Correlation must be between -1 and 1")

  K <- F1 + X
  w <- F1 / K
  sigma <- sqrt(sigma2^2 - 2 * rho * sigma1 * sigma2 * w + sigma1^2 * w^2)
  if (sigma == 0) stop("The spread volatility is zero; Kirk's approximation is undefined")
  s <- sigma * sqrt(T2M)
  D <- exp(-r * T2M)
  d1 <- (log(F2 / K) + s^2 / 2) / s
  d2 <- d1 - s

  call <- D * (F2 * stats::pnorm(d1) - K * stats::pnorm(d2))
  forward <- D * (F2 - K)

  sigma_w <- (sigma1^2 * w - rho * sigma1 * sigma2) / sigma
  sigma_ww <- (sigma1^2 - sigma_w^2) / sigma
  w_F1 <- X / K^2
  w_F1F1 <- -2 * X / K^3
  sigma_F1 <- sigma_w * w_F1
  sigma_F1F1 <- sigma_ww * w_F1^2 + sigma_w * w_F1F1

  vega_sigma <- D * F2 * sqrt(T2M) * stats::dnorm(d1)
  vanna_K <- D * stats::dnorm(d2) * d1 / sigma
  volga <- vega_sigma * d1 * d2 / sigma

  delta_F2 <- D * stats::pnorm(d1)
  delta_F1 <- -D * stats::pnorm(d2) + vega_sigma * sigma_F1
  gamma_F2 <- D * stats::dnorm(d1) / (F2 * s)
  gamma_F1 <- D * stats::dnorm(d2) / (K * s) + 2 * vanna_K * sigma_F1 + volga * sigma_F1^2 + vega_sigma * sigma_F1F1
  gamma_cross <- -D * stats::dnorm(d2) / (F2 * s) - sigma_F1 * D * sqrt(T2M) * stats::dnorm(d1) * d2 / s
  vega_1 <- vega_sigma * (sigma1 * w^2 - rho * sigma2 * w) / sigma
  vega_2 <- vega_sigma * (sigma2 - rho * sigma1 * w) / sigma
  theta <- r * call - D * F2 * stats::dnorm(d1) * sigma / (2 * sqrt(T2M))
  rho_r <- -T2M * call

  if (type == "put") {
    return(list(price = call - forward, delta_F1 = delta_F1 + D, delta_F2 = delta_F2 - D,
                gamma_F1 = gamma_F1, gamma_F2 = gamma_F2, gamma_cross = gamma_cross,
                vega_1 = vega_1, vega_2 = vega_2, theta = theta - r * forward, rho = rho_r + T2M * forward))
  }
  list(price = call, delta_F1 = delta_F1, delta_F2 = delta_F2,
       gamma_F1 = gamma_F1, gamma_F2 = gamma_F2, gamma_cross = gamma_cross,
       vega_1 = vega_1, vega_2 = vega_2, theta = theta, rho = rho_r)
}
