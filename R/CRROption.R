#' Cox-Ross-Rubinstein Option Pricing Model
#'
#' Computes the price of European and American options using the Cox-Ross-Rubinstein binomial model.
#' This function is optimized for performance using vectorized operations and pre-calculations.
#'
#' @param S Numeric, the current stock price (also known as the underlying asset price).
#' @param X Numeric, the strike price of the option.
#' @param sigma Numeric, the implied volatility of the underlying stock (annualized).
#' @param r Numeric, the risk-free interest rate (annualized).
#' @param b Numeric, the cost of carry, b = r - q for dividend paying assets, where q is the dividend yield rate.
#' @param T2M Numeric, the time to maturity of the option (in years).
#' @param N Integer, the number of time steps in the binomial tree.
#' @param type Character, the type of option ("call" or "put").
#' @param optionStyle Character, the style of the option ("european" or "american").
#'
#' @return A list containing the computed price of the option and a note indicating if the model is suitable for the provided parameters.
#'
#' @export
CRROption <- function(S, X, sigma, r, b, T2M, N, type, optionStyle) {
  dt <- T2M / N

  # Up and down factors adjusted for cost of carry 'b'
  u <- exp(sigma * sqrt(dt))      # Up factor
  d <- 1/u                       # Down factor

  # Risk-neutral probability 'q' considering 'b'
  q <- (exp(b * dt) - d) / (u - d)

  discount <- exp(-r * dt)

  asset_prices <- numeric(N + 1)
  option_values <- numeric(N + 1)

  for (i in 0:N) {
    asset_prices[i + 1] <- S * u^i * d^(N - i)
    if (type == "call") {
      option_values[i + 1] <- max(asset_prices[i + 1] - X, 0)
    } else if (type == "put") {
      option_values[i + 1] <- max(X - asset_prices[i + 1], 0)
    }
  }

  if (!optionStyle %in% c("european", "american")) {
    stop("Invalid option style. Please choose 'european' or 'american'.")
  }

  for (time_step in (N - 1):0) {
    for (j in 0:time_step) {
      hold_value <- (q * option_values[j + 2] + (1 - q) * option_values[j + 1]) * discount
      if (optionStyle == "american") {
        exercise_value <- if (type == "call") {
          max(S * u^j * d^(time_step - j) - X, 0)
        } else {
          max(X - S * u^j * d^(time_step - j), 0)
        }
        option_values[j + 1] <- max(hold_value, exercise_value)
      } else {
        option_values[j + 1] <- hold_value
      }
    }
    option_values <- option_values[1:(time_step + 1)]
  }

  note <- if (sigma > r * sqrt(dt)) {
    "Model parameters are acceptable."
  } else {
    "Consider adjusting your model parameters or using a different model for more accurate results."
  }

  return(list(price = option_values[1], note = note))
}

