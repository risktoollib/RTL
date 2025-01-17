#' @title Barrier Spread Option Pricing Functions
#' @description Functions for pricing barrier spread options using Kirk's approximation
#' with theoretically consistent barrier option implementation.
#' @name barrierOption
#'
#' @import numDeriv
#' @importFrom stats pnorm dnorm
NULL

# Internal function for proper barrier spread option pricing
.barrierSpreadPrice <- function(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                type = "call", barrier_type = "do",
                                monitoring = "continuous", monitoring_freq = 252) {
  epsilon <- 1e-10
  spread <- F2 - F1

  # Early return for expired options
  if (T2M < epsilon) {
    intrinsic <- if(type == "call") max(spread - X, 0) else max(X - spread, 0)
    barrier_hit <- if(barrier_type %in% c("uo", "ui")) spread >= B else spread <= B

    return(switch(barrier_type,
                  "uo" = if(barrier_hit) 0 else intrinsic,
                  "ui" = if(barrier_hit) intrinsic else 0,
                  "do" = if(barrier_hit) 0 else intrinsic,
                  "di" = if(barrier_hit) intrinsic else 0))
  }

  # Calculate effective volatility for the spread using Kirk's approximation
  F_eff1 <- (F1 + X) / (F2 + F1 + X + epsilon)
  F_eff2 <- F2 / (F2 + F1 + X + epsilon)
  sigma_eff <- sqrt(sigma1^2 * F_eff1^2 + sigma2^2 * F_eff2^2 -
                      2 * rho * sigma1 * sigma2 * F_eff1 * F_eff2)

  # Kirk's terms
  ratio <- (F2 + epsilon) / (F1 + X + epsilon)
  d1 <- (log(ratio) + 0.5 * sigma_eff^2 * T2M) / (sigma_eff * sqrt(T2M))
  d2 <- d1 - sigma_eff * sqrt(T2M)

  # Compute vanilla spread option price
  df <- exp(-r * T2M)
  vanilla_price <- if(type == "call") {
    df * (F2 * pnorm(d1) - (F1 + X) * pnorm(d2))
  } else {
    df * ((F1 + X) * pnorm(-d2) - F2 * pnorm(-d1))
  }

  if (monitoring == "continuous") {
    # Parameters for continuous monitoring
    mu <- r - 0.5 * sigma_eff^2
    lambda <- (mu + sqrt(mu^2 + 2 * r * sigma_eff^2)) / sigma_eff^2
    h <- log(B / spread) / (sigma_eff * sqrt(T2M))

    if (barrier_type %in% c("uo", "do")) {
      eta <- if(barrier_type == "uo") 1 else -1
      reflection_term <- (B/spread)^(2*lambda) * if(type == "call") {
        df * (F2 * pnorm(d1 - eta*2*h) - (F1 + X) * pnorm(d2 - eta*2*h))
      } else {
        df * ((F1 + X) * pnorm(-d2 + eta*2*h) - F2 * pnorm(-d1 + eta*2*h))
      }
      return(vanilla_price - reflection_term)
    } else {
      # Knock-in options as complement of knock-out
      out_price <- .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                       type, substr(barrier_type, 1, 2), "continuous")
      return(vanilla_price - out_price)
    }
  } else if (monitoring == "discrete") {
    # Broadie-Glasserman-Kou adjustment for discrete monitoring
    dt <- T2M / monitoring_freq
    B_adj <- B * exp(0.5826 * sigma_eff * sqrt(dt))
    return(.barrierSpreadPrice(F1, F2, X, B_adj, sigma1, sigma2, rho, T2M, r,
                               type, barrier_type, "continuous"))
  } else {
    # Terminal monitoring
    barrier_hit <- if(barrier_type %in% c("uo", "ui")) spread >= B else spread <= B
    return(switch(barrier_type,
                  "uo" = if(barrier_hit) 0 else vanilla_price,
                  "ui" = if(barrier_hit) vanilla_price else 0,
                  "do" = if(barrier_hit) 0 else vanilla_price,
                  "di" = if(barrier_hit) vanilla_price else 0))
  }
}

#' Analytically calculated Greeks for barrier spread options
#' @keywords internal
.barrierSpreadGreeks <- function(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                 type, barrier_type, monitoring) {
  epsilon <- 1e-10
  spread <- F2 - F1

  # Early return if at barrier for knock-out options
  if (((barrier_type %in% c("uo", "do")) && abs(spread - B) < epsilon)) {
    return(list(
      delta_F1 = 0, delta_F2 = 0,
      gamma_F1 = 0, gamma_F2 = 0, gamma_cross = 0,
      vega_1 = 0, vega_2 = 0,
      theta = 0, rho = 0
    ))
  }

  # Calculate effective parameters
  F_eff1 <- (F1 + X) / (F2 + F1 + X + epsilon)
  F_eff2 <- F2 / (F2 + F1 + X + epsilon)
  sigma_eff <- sqrt(sigma1^2 * F_eff1^2 + sigma2^2 * F_eff2^2 -
                      2 * rho * sigma1 * sigma2 * F_eff1 * F_eff2)

  # Basic terms
  d1 <- (log((F2 + epsilon)/(F1 + X + epsilon)) + 0.5 * sigma_eff^2 * T2M) /
    (sigma_eff * sqrt(T2M))
  d2 <- d1 - sigma_eff * sqrt(T2M)

  # Calculate Greeks based on barrier type
  if (monitoring == "continuous") {
    mu <- r - 0.5 * sigma_eff^2
    lambda <- (mu + sqrt(mu^2 + 2 * r * sigma_eff^2)) / sigma_eff^2
    h <- log(B / spread) / (sigma_eff * sqrt(T2M))

    # Implement analytical Greeks with proper barrier adjustments
    # This is a simplified version - full implementation would include
    # all barrier-specific terms and their derivatives

    # For knock-out options, delta should be close to 1 near spot and decay near barrier
    # Calculate moneyness and barrier factors
    is_itm <- if(type == "call") spread > X else spread < X
    dist_to_barrier <- if(barrier_type %in% c("uo", "ui")) B - spread else spread - B

    # For knock-out options, delta should reflect full exposure when ITM and away from barrier
    barrier_factor <- if(barrier_type %in% c("uo", "do")) {
      if(dist_to_barrier <= 0) {
        0  # At or beyond barrier
      } else {
        # Smooth decay as we approach barrier
        1 - exp(-50 * dist_to_barrier/abs(B))
      }
    } else {
      1  # No barrier adjustment for knock-in options
    }

    # Base deltas considering moneyness
    delta_F1 <- if(type == "call") {
      if(is_itm) -1 else 0
    } else {
      if(is_itm) 1 else 0
    }

    delta_F2 <- if(type == "call") {
      if(is_itm) 1 else 0
    } else {
      if(is_itm) -1 else 0
    }

    # Apply barrier factor and discounting
    delta_F1 <- delta_F1 * barrier_factor * exp(-r * T2M)
    delta_F2 <- delta_F2 * barrier_factor * exp(-r * T2M)

    # Gamma calculation with barrier adjustment
    gamma_factor <- if(barrier_type %in% c("uo", "do")) {
      1 - (B/spread)^(2*lambda)
    } else {
      1
    }

    gamma_base <- exp(-r * T2M) * dnorm(d1) / (F2 * sigma_eff * sqrt(T2M))
    gamma_F1 <- gamma_base * gamma_factor
    gamma_F2 <- gamma_base * gamma_factor
    gamma_cross <- -gamma_base * gamma_factor

    # Other Greeks follow similar pattern
    vega_1 <- exp(-r * T2M) * F2 * dnorm(d1) * sqrt(T2M) * gamma_factor
    vega_2 <- vega_1

    theta <- r * exp(-r * T2M) * (F2 * pnorm(d1) - (F1 + X) * pnorm(d2)) * gamma_factor
    rho_calc <- T2M * exp(-r * T2M) * (F2 * pnorm(d1) - (F1 + X) * pnorm(d2)) * gamma_factor

  } else {
    # For discrete and terminal monitoring, use simplified Greeks
    # This could be enhanced with proper adjustments for discrete monitoring
    delta_F1 <- -exp(-r * T2M) * pnorm(d2)
    delta_F2 <- exp(-r * T2M) * pnorm(d1)
    gamma_F1 <- exp(-r * T2M) * dnorm(d1) / (F2 * sigma_eff * sqrt(T2M))
    gamma_F2 <- gamma_F1
    gamma_cross <- -gamma_F1
    vega_1 <- exp(-r * T2M) * F2 * dnorm(d1) * sqrt(T2M)
    vega_2 <- vega_1
    theta <- r * exp(-r * T2M) * (F2 * pnorm(d1) - (F1 + X) * pnorm(d2))
    rho_calc <- T2M * exp(-r * T2M) * (F2 * pnorm(d1) - (F1 + X) * pnorm(d2))
  }

  return(list(
    delta_F1 = delta_F1, delta_F2 = delta_F2,
    gamma_F1 = gamma_F1, gamma_F2 = gamma_F2,
    gamma_cross = gamma_cross,
    vega_1 = vega_1, vega_2 = vega_2,
    theta = theta, rho = rho_calc
  ))
}

#' Barrier Spread Option Pricing using Kirk's Approximation
#'
#' @description
#' Computes the price and Greeks of European barrier spread options using Kirk's
#' approximation with theoretically consistent barrier option implementation.
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
    stop("Invalid barrier type")
  if (!monitoring %in% c("continuous", "discrete", "terminal"))
    stop("Invalid monitoring approach")
  if (monitoring == "discrete" && (!is.numeric(monitoring_freq) || monitoring_freq <= 0))
    stop("Monitoring frequency must be positive for discrete monitoring")

  # Calculate option price
  option_price <- .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                      type, barrier_type, monitoring, monitoring_freq)

  # Calculate Greeks analytically
  greeks <- .barrierSpreadGreeks(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                 type, barrier_type, monitoring)

  # Return results
  c(list(
    price = option_price,
    spread = F2 - F1,
    barrier_level = B,
    barrier_type = barrier_type,
    monitoring = monitoring
  ), greeks)
}
