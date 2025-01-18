#' @title Enhanced Barrier Spread Option Pricing
#' @description Pricing barrier spread options with enhanced numerical stability and robustness for commodity pipeline spreads.
#' @param F1 numeric, forward price of the first asset (e.g., pipeline origin price)
#' @param F2 numeric, forward price of the second asset (e.g., pipeline destination price)
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
#' @name barrierSpreadOption
#' @export barrierSpreadOption
#' @examples
#' barrierSpreadOption(
#'   F1 = -12.00, F2 = -5.00, X = 4.00, B = 9.5,
#'   sigma1 = 0.3, sigma2 = 0.3, rho = 0.9, r = 0.01,
#'   T2M = 1, type = "call", barrier_type = "uo", monitoring = "continuous"
#' )
#'
#' @importFrom stats pnorm dnorm
#' @importFrom numDeriv grad hessian

NULL

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

  epsilon <- .Machine$double.eps
  spread <- F2 - F1

  # Early exit for expired options
  if (T2M < epsilon) {
    payoff <- if (type == "call") {
      if (barrier_type == "uo" && spread >= B) {
        0
      } else {
        max(0, spread - X)
      }
    } else {
      if (barrier_type == "do" && spread <= B) {
        0
      } else {
        max(0, X - spread)
      }
    }
    return(list(
      price = payoff,
      delta_F1 = if (spread > X && spread < B) -1 else 0,
      delta_F2 = if (spread > X && spread < B) 1 else 0,
      gamma_F1 = 0, gamma_F2 = 0,
      gamma_cross = 0, vega_1 = 0, vega_2 = 0,
      theta = 0, rho = 0
    ))
  }

  # Calculate spread volatility
  sigma_spread <- sqrt(sigma1^2 + sigma2^2 - 2 * rho * sigma1 * sigma2)

  # Standard Black-Scholes terms for spread option
  d1 <- (log((F2 - F1)/X) + 0.5 * sigma_spread^2 * T2M) / (sigma_spread * sqrt(T2M))
  d2 <- d1 - sigma_spread * sqrt(T2M)

  # Calculate barrier effect
  if (monitoring == "continuous") {
    barrier_distance <- (B - spread) / (B * sigma_spread * sqrt(T2M))
    barrier_effect <- pnorm(barrier_distance)
  } else if (monitoring == "terminal") {
    barrier_effect <- if (spread < B) 1 else 0
  } else {
    barrier_effect <- 1  # Default for discrete monitoring
  }

  # Calculate vanilla spread option price
  df <- exp(-r * T2M)
  vanilla_price <- if (type == "call") {
    df * ((F2 - F1) * pnorm(d1) - X * pnorm(d2))
  } else {
    df * (X * pnorm(-d2) - (F2 - F1) * pnorm(-d1))
  }

  # Apply barrier effect to price
  price <- vanilla_price * barrier_effect
  price <- max(0, price)  # Ensure non-negative price

  # Calculate deltas for call spread option
  if (type == "call") {
    delta_F1 <- -df * pnorm(d1) * barrier_effect  # Note: Changed from d2 to d1
    delta_F2 <- df * pnorm(d1) * barrier_effect
  } else {
    delta_F1 <- df * pnorm(-d1) * barrier_effect
    delta_F2 <- -df * pnorm(-d1) * barrier_effect
  }

  # Calculate other Greeks
  gamma_base <- df * dnorm(d1) / (spread * sigma_spread * sqrt(T2M))
  gamma <- gamma_base * barrier_effect

  vega <- gamma * spread * sqrt(T2M) * barrier_effect
  vega_1 <- vega * (sigma1 - rho * sigma2)
  vega_2 <- vega * (sigma2 - rho * sigma1)

  theta <- -df * (
    spread * dnorm(d1) * sigma_spread / (2 * sqrt(T2M)) +
      r * (spread * pnorm(d1) - X * pnorm(d2))
  ) * barrier_effect

  rho_calc <- T2M * price

  list(
    price = price,
    delta_F1 = delta_F1,
    delta_F2 = delta_F2,
    gamma_F1 = gamma,
    gamma_F2 = gamma,
    gamma_cross = -gamma,
    vega_1 = vega_1,
    vega_2 = vega_2,
    theta = theta,
    rho = rho_calc
  )
}
