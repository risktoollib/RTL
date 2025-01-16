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
#' Kirk's approximation is particularly useful for spread options where the exercise
#' price is zero or small relative to the asset prices. The approximation assumes
#' that the ratio of the assets follows a lognormal distribution.
#'
#' The implementation includes a small constant (epsilon) to avoid numerical
#' instabilities that might arise from division by zero.
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
  # Input validation
  if (!type %in% c("call", "put"))
    stop("Type must be 'call' or 'put'")

  # Small constant to avoid division by zero
  epsilon <- 1e-10

  # Effective forward price weights
  F_eff1 <- F1 / (F1 + F2 + epsilon)
  F_eff2 <- F2 / (F1 + F2 + epsilon)

  # Effective volatility of the spread
  sigma_eff <- sqrt((sigma1^2 * F_eff1^2) + (sigma2^2 * F_eff2^2) -
                      2 * rho * sigma1 * sigma2 * F_eff1 * F_eff2 + epsilon)

  # Kirk's d1 and d2
  d1 <- (log((F2 + epsilon) / (F1 + X + epsilon)) +
           (0.5 * sigma_eff^2) * T2M) / (sigma_eff * sqrt(T2M + epsilon))
  d2 <- d1 - sigma_eff * sqrt(T2M + epsilon)

  # Option price calculation based on type
  if (type == "call") {
    option_price <- exp(-r * T2M) * ((F2 * pnorm(d1)) - ((F1 + X) * pnorm(d2)))
    delta_F1 <- -exp(-r * T2M) * pnorm(d2)
    delta_F2 <- exp(-r * T2M) * pnorm(d1)
  } else {  # put option
    option_price <- exp(-r * T2M) * ((F1 + X) * pnorm(-d2) - F2 * pnorm(-d1))
    delta_F1 <- exp(-r * T2M) * pnorm(-d2)
    delta_F2 <- -exp(-r * T2M) * pnorm(-d1)
  }

  # Gamma calculations (same for both call and put due to put-call parity)
  gamma_F1 <- exp(-r * T2M) * dnorm(d2) / ((F1 + X) * sigma_eff * sqrt(T2M))
  gamma_F2 <- exp(-r * T2M) * dnorm(d1) / (F2 * sigma_eff * sqrt(T2M))
  gamma_cross <- -exp(-r * T2M) * dnorm(d2) * (
    1 / ((F1 + X) * sigma_eff * sqrt(T2M))
  )

  # Vega calculations (same for both call and put)
  vega_1 <- exp(-r * T2M) * F2 * sqrt(T2M) * dnorm(d1) *
    (sigma1 * F_eff1^2 - rho * sigma2 * F_eff1 * F_eff2) / sigma_eff
  vega_2 <- exp(-r * T2M) * F2 * sqrt(T2M) * dnorm(d1) *
    (sigma2 * F_eff2^2 - rho * sigma1 * F_eff1 * F_eff2) / sigma_eff

  # Theta calculation
  if (type == "call") {
    theta <- -exp(-r * T2M) * (
      F2 * dnorm(d1) * sigma_eff / (2 * sqrt(T2M)) -
        (F1 + X) * dnorm(d2) * sigma_eff / (2 * sqrt(T2M)) +
        r * ((F2 * pnorm(d1)) - ((F1 + X) * pnorm(d2)))
    )
  } else {  # put option
    theta <- -exp(-r * T2M) * (
      F2 * dnorm(d1) * sigma_eff / (2 * sqrt(T2M)) -
        (F1 + X) * dnorm(d2) * sigma_eff / (2 * sqrt(T2M)) -
        r * ((F1 + X) * pnorm(-d2) - F2 * pnorm(-d1))
    )
  }

  # Rho calculation
  rho <- T2M * option_price

  # Return all greeks in a list
  return(list(
    price = option_price,
    delta_F1 = delta_F1,
    delta_F2 = delta_F2,
    gamma_F1 = gamma_F1,
    gamma_F2 = gamma_F2,
    gamma_cross = gamma_cross,
    vega_1 = vega_1,
    vega_2 = vega_2,
    theta = theta,
    rho = rho
  ))
}
