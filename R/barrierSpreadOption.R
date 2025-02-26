#' @title  Barrier Spread Option Pricing
#' @description This model applies Kirk's (1995) closed-form approximation for pricing spread options,
#'   incorporating barrier adjustments for continuous and terminal monitoring.
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
#' @param barrier_type character, "do" or "uo" (down-and-out, up-and-out)
#' @param monitoring character, "continuous" or "terminal"
#' @return A list containing option price and Greeks.
#' @import numDeriv
#' @name barrierSpreadOption
#' @export barrierSpreadOption

barrierSpreadOption <- function(F1 = -12, F2 = -3, X = 5.5, B = 9, sigma1 = 0.6, sigma2 = 0.6, rho = .3, T2M = 1/12, r = 0.045,
                                type = "call", barrier_type = "uo",
                                monitoring = "continuous") {
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
  if (!barrier_type %in% c("do", "uo"))
    stop("Invalid barrier type")
  if (!monitoring %in% c("continuous", "terminal"))
    stop("Invalid monitoring approach")
  if ((type == "call" && barrier_type != "uo") || (type == "put" && barrier_type != "do"))
    stop("Only up-and-out calls and down-and-out puts are supported")

  epsilon <- 1e-10
  spread <- F2 - F1
  sigma_spread <- sqrt(sigma1^2 + sigma2^2 - 2 * rho * sigma1 * sigma2)

  # Zero output template
  zero_output <- list(
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
  )

  # Check if option is knocked out based on barrier condition
  if ((type == "call" && spread >= B) || (type == "put" && spread <= B))
    return(zero_output)

  # Early exit for expired options
  if (T2M < epsilon) {
    if (type == "call") {
      payoff <- max(0, spread - X)
    } else {  # put
      payoff <- max(0, X - spread)
    }

    # Ensure very small payoffs are set to exactly zero
    if (abs(payoff) < epsilon) payoff <- 0

    zero_output$price <- payoff
    return(zero_output)
  }

  # Standard Black-Scholes terms
  d1 <- (log(spread/X) + 0.5 * sigma_spread^2 * T2M) / (sigma_spread * sqrt(T2M))
  d2 <- d1 - sigma_spread * sqrt(T2M)

  # Barrier terms
  b1 <- (log(spread/B) + 0.5 * sigma_spread^2 * T2M) / (sigma_spread * sqrt(T2M))
  b2 <- b1 - sigma_spread * sqrt(T2M)

  # Price calculation
  df <- exp(-r * T2M)
  if (type == "call") {
    power <- (B/spread)^2
    vanilla <- df * (spread * pnorm(d1) - X * pnorm(d2))
    reflection <- df * power * (spread * pnorm(b1) - X * pnorm(b2))
    price <- max(0, vanilla - reflection)

    # Greeks calculations if barrier not breached
    if (spread < B) {
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

      return(list(
        price = round(price, 2),
        delta_F1 = delta_F1,
        delta_F2 = delta_F2,
        gamma_F1 = gamma_F1,
        gamma_F2 = gamma_F2,
        gamma_cross = gamma_cross,
        vega_1 = vega_1,
        vega_2 = vega_2,
        theta = theta,
        rho = rho_calc
      ))
    }
  } else {  # put
    # For down-and-out put, follow derivmkts approach
    power <- (spread/B)^2
    vanilla <- df * (X * pnorm(-d2) - spread * pnorm(-d1))
    reflection <- df * power * (X * pnorm(-b2) - spread * pnorm(-b1))
    price <- max(0, vanilla - reflection)

    # Greeks calculations if barrier not breached (spread > B)
    if (spread > B) {
      delta_F1 <- df * (pnorm(-d1) - power * pnorm(-b1))
      delta_F2 <- -df * (pnorm(-d1) - power * pnorm(-b1))

      gamma_base <- df * (
        dnorm(d1)/(spread * sigma_spread * sqrt(T2M)) -
          power * dnorm(b1)/(spread * sigma_spread * sqrt(T2M))
      )

      gamma_F1 <- gamma_base
      gamma_F2 <- gamma_base
      gamma_cross <- -gamma_base

      vega_1 <- spread * df * (
        dnorm(-d1) * sqrt(T2M) - power * dnorm(-b1) * sqrt(T2M)
      ) * (sigma1 - rho * sigma2) / sigma_spread

      vega_2 <- spread * df * (
        dnorm(-d1) * sqrt(T2M) - power * dnorm(-b1) * sqrt(T2M)
      ) * (sigma2 - rho * sigma1) / sigma_spread

      theta <- -0.5 * sigma_spread^2 * spread^2 * gamma_base +
        r * (X * pnorm(-d2) - spread * pnorm(-d1)) * df -
        r * power * (X * pnorm(-b2) - spread * pnorm(-b1)) * df

      rho_calc <- -T2M * (
        X * df * (pnorm(-d2) - power * pnorm(-b2))
      )

      return(list(
        price = round(price, 2),
        delta_F1 = delta_F1,
        delta_F2 = delta_F2,
        gamma_F1 = gamma_F1,
        gamma_F2 = gamma_F2,
        gamma_cross = gamma_cross,
        vega_1 = vega_1,
        vega_2 = vega_2,
        theta = theta,
        rho = rho_calc
      ))
    }
  }

  # Return zero output if no other conditions met
  zero_output$price <- round(price, 2)
  return(zero_output)
}
