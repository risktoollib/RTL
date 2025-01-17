#' @title Barrier Spread Option Pricing Functions
#' @description Functions for pricing barrier spread options using Kirk's approximation
#' with proper barrier option theory implementation.
#' @name barrierOption
#'
#' @importFrom stats pnorm dnorm
#' @importFrom numDeriv grad hessian
NULL

# Internal function for proper barrier spread option pricing
.barrierSpreadPrice <- function(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                type = "call", barrier_type = "do",
                                monitoring = "continuous", monitoring_freq = 252) {
  epsilon <- 1e-10
  spread <- F2 - F1

  if (T2M < epsilon) {
    # At expiry, just check intrinsic and barrier
    intrinsic <- if(type == "call") max(spread - X, 0) else max(X - spread, 0)
    barrier_hit <- if(barrier_type %in% c("uo", "ui")) {
      spread >= B
    } else {
      spread <= B
    }

    if (barrier_hit) {
      return(switch(barrier_type,
                    "uo" = 0, "ui" = intrinsic,
                    "do" = 0, "di" = intrinsic
      ))
    } else {
      return(switch(barrier_type,
                    "uo" = intrinsic, "ui" = 0,
                    "do" = intrinsic, "di" = 0
      ))
    }
  }

  # Calculate effective volatility for the spread
  F_eff1 <- (F1 + X) / (F2 + F1 + X + epsilon)
  F_eff2 <- F2 / (F2 + F1 + X + epsilon)
  sigma_eff <- sqrt((sigma1^2 * F_eff1^2) + (sigma2^2 * F_eff2^2) -
                      2 * rho * sigma1 * sigma2 * F_eff1 * F_eff2)

  # Kirk's terms for vanilla spread
  ratio <- (F2 + epsilon) / (F1 + X + epsilon)
  d1 <- (log(ratio) + (0.5 * sigma_eff^2) * T2M) / (sigma_eff * sqrt(T2M))
  d2 <- d1 - sigma_eff * sqrt(T2M)

  # Vanilla spread option price
  vanilla_price <- if(type == "call") {
    exp(-r * T2M) * (F2 * pnorm(d1) - (F1 + X) * pnorm(d2))
  } else {
    exp(-r * T2M) * ((F1 + X) * pnorm(-d2) - F2 * pnorm(-d1))
  }

  if (monitoring == "terminal") {
    # Terminal monitoring only checks barrier at expiry
    barrier_hit <- if(barrier_type %in% c("uo", "ui")) {
      spread >= B
    } else {
      spread <= B
    }

    if (barrier_hit) {
      return(switch(barrier_type,
                    "uo" = 0, "ui" = vanilla_price,
                    "do" = 0, "di" = vanilla_price
      ))
    } else {
      return(switch(barrier_type,
                    "uo" = vanilla_price, "ui" = 0,
                    "do" = vanilla_price, "di" = 0
      ))
    }

  } else if (monitoring == "continuous") {
    # For continuous monitoring, use reflection principle
    mu <- (r - 0.5 * sigma_eff^2)
    lambda <- (mu + sqrt(mu^2 + 2 * r * sigma_eff^2)) / sigma_eff^2

    if (barrier_type %in% c("uo", "do")) {
      # Out options use reflection
      eta <- if(barrier_type == "uo") 1 else -1
      h <- log(B / spread) / (sigma_eff * sqrt(T2M))
      k <- -2 * mu * h / sigma_eff^2

      reflection_price <- (B/spread)^(2*lambda) * if(type == "call") {
        exp(-r * T2M) * (F2 * pnorm(d1 - eta*2*h) -
                           (F1 + X) * pnorm(d2 - eta*2*h))
      } else {
        exp(-r * T2M) * ((F1 + X) * pnorm(-d2 + eta*2*h) -
                           F2 * pnorm(-d1 + eta*2*h))
      }

      return(vanilla_price - reflection_price)

    } else {
      # In options are complement of out options
      out_price <- .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                       type, substr(barrier_type, 1, 2), "continuous")
      return(vanilla_price - out_price)
    }

  } else {
    # Discrete monitoring using Broadie-Glasserman-Kou adjustment
    dt <- T2M / monitoring_freq
    B_adj <- B * exp(0.5826 * sigma_eff * sqrt(dt))

    # Recurse with adjusted barrier
    return(.barrierSpreadPrice(F1, F2, X, B_adj, sigma1, sigma2, rho, T2M, r,
                               type, barrier_type, "continuous"))
  }
}

#' Barrier Spread Option Pricing using Kirk's Approximation
#'
#' @description
#' Computes the price and Greeks of European barrier spread options using Kirk's
#' approximation with proper barrier option theory implementation.
#'
#' @param F1 numeric, the forward price of the first asset
#' @param F2 numeric, the forward price of the second asset
#' @param X numeric, the strike price of the spread option
#' @param B numeric, the barrier level for the spread (F2 - F1)
#' @param sigma1 numeric, the volatility of the first asset (annualized)
#' @param sigma2 numeric, the volatility of the second asset (annualized)
#' @param rho numeric, the correlation coefficient between the two assets
#' @param T2M numeric, the time to maturity in years
#' @param r numeric, the risk-free interest rate (annualized)
#' @param type character, type of option ("call" or "put")
#' @param barrier_type character, type of barrier ("do", "di", "uo", "ui")
#' @param monitoring character, barrier monitoring ("continuous", "discrete", "terminal")
#' @param monitoring_freq numeric, frequency for discrete monitoring (default: 252)
#'
#' @return A list containing option price and Greeks
#' @export
barrierSpreadOption <- function(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                type = "call", barrier_type = "do",
                                monitoring = "continuous", monitoring_freq = 252) {
  # Input validation
  if (!is.numeric(F1) || !is.numeric(F2) || !is.numeric(X) || !is.numeric(B))
    stop("Asset prices, strike price, and barrier level must be numeric")
  if (!is.numeric(sigma1) || !is.numeric(sigma2))
    stop("Volatilities must be numeric")
  if (sigma1 <= 0 || sigma2 <= 0)
    stop("Volatilities must be positive")
  if (!is.numeric(rho) || abs(rho) > 1)
    stop("Correlation must be between -1 and 1")
  if (!is.numeric(T2M) || T2M < 0)
    stop("Time to maturity must be non-negative")
  if (!is.numeric(r))
    stop("Interest rate must be numeric")
  if (!type %in% c("call", "put"))
    stop("Type must be 'call' or 'put'")
  if (!barrier_type %in% c("do", "di", "uo", "ui"))
    stop("Invalid barrier type. Must be 'do', 'di', 'uo', or 'ui'")
  if (!monitoring %in% c("continuous", "discrete", "terminal"))
    stop("Invalid monitoring approach")
  if (monitoring == "discrete" && (!is.numeric(monitoring_freq) || monitoring_freq <= 0))
    stop("Monitoring frequency must be positive for discrete monitoring")

  # Calculate option price
  option_price <- .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                      type, barrier_type, monitoring, monitoring_freq)

  # Calculate Greeks using numerical derivatives
  delta_F1 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(x, F2, X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, F1)

  delta_F2 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, x, X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, F2)

  gamma_F1 <- numDeriv::hessian(function(x) {
    .barrierSpreadPrice(x, F2, X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, F1)[1,1]

  gamma_F2 <- numDeriv::hessian(function(x) {
    .barrierSpreadPrice(F1, x, X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, F2)[1,1]

  gamma_cross <- numDeriv::hessian(function(x) {
    .barrierSpreadPrice(x[1], x[2], X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, c(F1, F2))[1,2]

  vega_1 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, x, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, sigma1)

  vega_2 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, sigma1, x, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, sigma2)

  theta <- -numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, x, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, T2M)

  rho_calc <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, x,
                        type, barrier_type, monitoring, monitoring_freq)
  }, r)

  # Return results
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
    rho = rho_calc,
    spread = F2 - F1,
    barrier_level = B,
    barrier_type = barrier_type,
    monitoring = monitoring
  ))
}
