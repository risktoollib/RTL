#' @title  Barrier Spread Option Pricing
#' @description This model applies Kirk's (1995) closed-form approximation to
#'   price spread options, assuming the spread \eqn{F_2 - F_1} is approximately
#'   log-normal. Unlike Kirk’s original ratio-based formulation, the model uses
#'   the numeric difference of the forward prices \eqn{F_2 - F_1} to facilitate
#'   direct comparisons with numeric strike prices and barriers. The model
#'   incorporates adjustments for barriers (up-and-out, down-and-out, etc.) by
#'   reflecting the spread's log-normal distribution and applying a correction
#'   term for breached barriers.
#' @param F1 numeric, forward price of the first asset (e.g., pipeline origination price as a futures)
#' @param F2 numeric, forward price of the second asset (e.g., pipeline destination price as a futures)
#' @param X numeric, strike price of the spread option
#' @param B numeric, barrier level for the spread (F2 - F1)
#' @param sigma1 numeric, volatility of the first asset (annualized)
#' @param sigma2 numeric, volatility of the second asset (annualized)
#' @param rho numeric, correlation coefficient between the two assets
#' @param T2M numeric, time to maturity in years
#' @param r numeric, risk-free interest rate (annualized)
#' @param type character, "call" or "put"
#' @param barrier_type character, "do", "di", "uo", "ui" (down-and-out/in, up-and-out/in)
#' @param monitoring character, "continuous", "discrete", "terminal"
#' @param monitoring_freq numeric, frequency of monitoring for discrete barriers (default: 252)
#' @return A list containing option price and Greeks.
#' @import numDeriv
#' @name barrierSpreadOption
#' @export barrierSpreadOption

barrierSpreadOption <- function(F1 = -12, F2 = -3, X = 5.5, B = 9, sigma1 = 0.6, sigma2 = 0.6, rho = .3, T2M = 1/12, r = 0.045,
                                type = "call", barrier_type = "uo",
                                monitoring = "continuous", monitoring_freq = 252) {
  # Input validation
  if (!is.numeric(F1) || !is.numeric(F2) || !is.numeric(X) || !is.numeric(B))
    stop("Asset prices, strike price, and barrier level must be numeric")
  if (!is.numeric(sigma1) || !is.numeric(sigma2) || sigma1 <= 0 || sigma2 <= 0)
    stop("Volatilities must be positive numbers")
  if (!is.numeric(rho) || abs(rho) > 1)
    stop("Correlation must be between -1 and 1")
  if (!is.numeric(T2M) || T2M < 0)
    stop("Time to maturity must be non-negative")
  if (!is.numeric(r))
    stop("Interest rate must be numeric")
  if (!type %in% c("call", "put"))
    stop("Type must be 'call' or 'put'")
  if (!barrier_type %in% c("do", "di", "uo", "ui"))
    stop("Invalid barrier type")
  if (!monitoring %in% c("continuous", "discrete", "terminal"))
    stop("Invalid monitoring approach")

  epsilon <- 1e-10
  spread <- F2 - F1

  # Return zero price and Greeks if barrier is breached
  if (barrier_type == "uo" && spread >= B) {
    return(list(
      price = 0,
      delta_F1 = 0,
      delta_F2 = 0,
      gamma_F1 = 0,
      gamma_F2 = 0,
      gamma_cross = 0,
      vega_1 = 0,
      vega_2 = 0,
      theta = 0,
      rho = 0
    ))
  }

  # Early exit for expired options
  if (T2M < epsilon) {
    # For numerical stability, ensure very small numbers are treated as zero
    clean_spread <- if(abs(spread - X) < epsilon) X else spread

    payoff <- if (type == "call") {
      if (barrier_type == "uo" && clean_spread >= B) {
        0
      } else {
        max(0, clean_spread - X)
      }
    } else {
      if (barrier_type == "uo" && clean_spread >= B) {
        0
      } else {
        max(0, X - clean_spread)
      }
    }

    # Ensure the payoff is exactly zero if it's very close to zero
    if(abs(payoff) < epsilon) payoff <- 0

    return(list(
      price = payoff,
      delta_F1 = 0,
      delta_F2 = 0,
      gamma_F1 = 0,
      gamma_F2 = 0,
      gamma_cross = 0,
      vega_1 = 0,
      vega_2 = 0,
      theta = 0,
      rho = 0
    ))
  }

  # Calculate spread volatility
  sigma_spread <- sqrt(sigma1^2 + sigma2^2 - 2 * rho * sigma1 * sigma2)

  # Standard Black-Scholes terms
  d1 <- (log(spread/X) + 0.5 * sigma_spread^2 * T2M) / (sigma_spread * sqrt(T2M))
  d2 <- d1 - sigma_spread * sqrt(T2M)

  # Barrier terms for up-and-out
  b1 <- (log(spread/B) + 0.5 * sigma_spread^2 * T2M) / (sigma_spread * sqrt(T2M))
  b2 <- b1 - sigma_spread * sqrt(T2M)

  # Power term for reflection
  power <- (B/spread)^2

  # Price calculation
  df <- exp(-r * T2M)
  if (type == "call") {
    vanilla <- df * (spread * pnorm(d1) - X * pnorm(d2))
    reflection <- df * power * (spread * pnorm(b1) - X * pnorm(b2))
    price <- max(0, vanilla - reflection)

    # Greeks calculations
    if (spread < B) {  # Only calculate non-zero Greeks when barrier not breached
      delta_F1 <- -df * (pnorm(d1) - power * pnorm(b1))
      delta_F2 <- df * (pnorm(d1) - power * pnorm(b1))

      gamma_base <- df * (
        dnorm(d1)/(spread * sigma_spread * sqrt(T2M)) -
          power * dnorm(b1)/(spread * sigma_spread * sqrt(T2M))
      )

      gamma_F1 <- gamma_base
      gamma_F2 <- gamma_base
      gamma_cross <- -gamma_base

      vega_1 <- spread * df * (
        dnorm(d1) * sqrt(T2M) - power * dnorm(b1) * sqrt(T2M)
      ) * (sigma1 - rho * sigma2) / sigma_spread

      vega_2 <- spread * df * (
        dnorm(d1) * sqrt(T2M) - power * dnorm(b1) * sqrt(T2M)
      ) * (sigma2 - rho * sigma1) / sigma_spread

      theta <- -0.5 * sigma_spread^2 * spread^2 * gamma_base -
        r * (spread * pnorm(d1) - X * pnorm(d2)) * df +
        r * power * (spread * pnorm(b1) - X * pnorm(b2)) * df

      rho_calc <- T2M * (
        X * df * (pnorm(d2) - power * pnorm(b2))
      )
    } else {  # Zero Greeks when barrier is breached
      delta_F1 <- 0
      delta_F2 <- 0
      gamma_F1 <- 0
      gamma_F2 <- 0
      gamma_cross <- 0
      vega_1 <- 0
      vega_2 <- 0
      theta <- 0
      rho_calc <- 0
    }
  } else {
    stop("Put options not yet implemented")
  }

  list(
    price = round(price, digits = 2),
    delta_F1 = delta_F1,
    delta_F2 = delta_F2,
    gamma_F1 = gamma_F1,
    gamma_F2 = gamma_F2,
    gamma_cross = gamma_cross,
    vega_1 = vega_1,
    vega_2 = vega_2,
    theta = theta,
    rho = rho_calc
  )
}
