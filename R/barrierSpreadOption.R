#' @title Barrier Spread Option Pricing Functions
#' @description Functions for pricing barrier spread options using Kirk's approximation
#' @name barrierOption
#'
#' @importFrom stats pnorm dnorm
#' @importFrom numDeriv grad hessian
NULL

# Internal function for price calculation only
.barrierSpreadPrice <- function(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                type = "call", barrier_type = "do",
                                monitoring = "continuous", monitoring_freq = 252) {
  # Small constant to avoid division by zero
  epsilon <- 1e-10

  # Effective forward price weights
  F_eff1 <- F1 / (F1 + F2 + epsilon)
  F_eff2 <- F2 / (F1 + F2 + epsilon)

  # Effective volatility of the spread
  sigma_eff <- sqrt((sigma1^2 * F_eff1^2) + (sigma2^2 * F_eff2^2) -
                      2 * rho * sigma1 * sigma2 * F_eff1 * F_eff2 + epsilon)

  # Modified parameters for barrier consideration
  mu <- (r - 0.5 * sigma_eff^2)

  # Barrier adjustment terms
  lambda <- (mu + sqrt(mu^2 + 2 * r * sigma_eff^2)) / sigma_eff^2

  # Basic Kirk's terms
  d1 <- (log((F2 + epsilon) / (F1 + X + epsilon)) +
           (0.5 * sigma_eff^2) * T2M) / (sigma_eff * sqrt(T2M + epsilon))
  d2 <- d1 - sigma_eff * sqrt(T2M + epsilon)

  # Barrier monitoring adjustment
  if (monitoring == "discrete") {
    dt <- T2M / monitoring_freq
    B <- B * exp(0.5826 * sigma_eff * sqrt(dt))
  } else if (monitoring == "terminal") {
    barrier_type <- ifelse(F2 > B, "ui", "di")
  }

  # Barrier terms
  x1 <- log((F2 + epsilon) / (B * (F1 + X) + epsilon)) / (sigma_eff * sqrt(T2M)) +
    lambda * sigma_eff * sqrt(T2M)
  x2 <- x1 - sigma_eff * sqrt(T2M)
  y1 <- log((B^2 * (F1 + X)^2) / (F2 * (F1 + X) + epsilon)) /
    (sigma_eff * sqrt(T2M)) + lambda * sigma_eff * sqrt(T2M)
  y2 <- y1 - sigma_eff * sqrt(T2M)

  # Calculate vanilla option price
  vanilla_price <- if(type == "call") {
    exp(-r * T2M) * ((F2 * pnorm(d1)) - ((F1 + X) * pnorm(d2)))
  } else {
    exp(-r * T2M) * ((F1 + X) * pnorm(-d2) - F2 * pnorm(-d1))
  }

  # Price calculation based on barrier type and option type
  option_price <- switch(barrier_type,
                         "do" = {
                           if(type == "call") {
                             exp(-r * T2M) * (
                               (F2 * (pnorm(d1) - (F2/B)^(2*lambda) * pnorm(y1))) -
                                 ((F1 + X) * (pnorm(d2) - (F2/B)^(2*lambda-2) * pnorm(y2)))
                             )
                           } else {
                             exp(-r * T2M) * (
                               ((F1 + X) * (pnorm(-d2) - (F2/B)^(2*lambda-2) * pnorm(-y2))) -
                                 (F2 * (pnorm(-d1) - (F2/B)^(2*lambda) * pnorm(-y1)))
                             )
                           }
                         },
                         "di" = {
                           vanilla_price - option_price
                         },
                         "uo" = {
                           if(type == "call") {
                             exp(-r * T2M) * (
                               (F2 * (pnorm(-d1) - (F2/B)^(2*lambda) * pnorm(-y1))) -
                                 ((F1 + X) * (pnorm(-d2) - (F2/B)^(2*lambda-2) * pnorm(-y2)))
                             )
                           } else {
                             exp(-r * T2M) * (
                               ((F1 + X) * (pnorm(d2) - (F2/B)^(2*lambda-2) * pnorm(y2))) -
                                 (F2 * (pnorm(d1) - (F2/B)^(2*lambda) * pnorm(y1)))
                             )
                           }
                         },
                         "ui" = {
                           vanilla_price - option_price
                         }
  )

  return(option_price)
}

#' Enhanced Barrier Spread Option using Extended Kirk's Approximation
#'
#' @description
#' Computes the price and Greeks of European barrier spread options using an extension
#' of Kirk's 1995 approximation following Bjerksund & Stensland (2011) approach.
#' Supports both call and put options with various barrier types and monitoring approaches.
#'
#' @param F1 numeric, the forward price of the first asset
#' @param F2 numeric, the forward price of the second asset
#' @param X numeric, the strike price of the spread option
#' @param B numeric, the barrier level
#' @param sigma1 numeric, the volatility of the first asset (annualized)
#' @param sigma2 numeric, the volatility of the second asset (annualized)
#' @param rho numeric, the correlation coefficient between the two assets (-1 <= rho <= 1)
#' @param T2M numeric, the time to maturity in years
#' @param r numeric, the risk-free interest rate (annualized)
#' @param type character, type of option ("call" or "put")
#' @param barrier_type character, type of barrier ("do" for down-and-out, "di" for down-and-in,
#'                     "uo" for up-and-out, "ui" for up-and-in)
#' @param monitoring character, barrier monitoring approach ("continuous", "discrete", "terminal")
#' @param monitoring_freq numeric, frequency of monitoring for discrete monitoring (default: 252 for daily)
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \code{price}: The price of the barrier spread option
#'   \item \code{delta_F1}: The sensitivity of the option price to changes in F1
#'   \item \code{delta_F2}: The sensitivity of the option price to changes in F2
#'   \item \code{gamma_F1}: The second derivative of the option price with respect to F1
#'   \item \code{gamma_F2}: The second derivative of the option price with respect to F2
#'   \item \code{gamma_cross}: The mixed second derivative with respect to F1 and F2
#'   \item \code{vega_1}: The sensitivity of the option price to changes in sigma1
#'   \item \code{vega_2}: The sensitivity of the option price to changes in sigma2
#'   \item \code{theta}: The sensitivity of the option price to the passage of time
#'   \item \code{rho}: The sensitivity of the option price to changes in the interest rate
#'   \item \code{barrier_level}: The specified barrier level
#'   \item \code{barrier_type}: The type of barrier option
#'   \item \code{monitoring}: The barrier monitoring approach used
#' }
#'
#' @details
#' The function implements a barrier version of Kirk's approximation for spread options.
#' It includes support for different barrier types and monitoring approaches:
#'
#' Barrier Types:
#' \itemize{
#'   \item Down-and-out ("do"): Option becomes worthless if asset price hits barrier from above
#'   \item Down-and-in ("di"): Option activates when asset price hits barrier from above
#'   \item Up-and-out ("uo"): Option becomes worthless if asset price hits barrier from below
#'   \item Up-and-in ("ui"): Option activates when asset price hits barrier from below
#' }
#'
#' Monitoring Approaches:
#' \itemize{
#'   \item Continuous: Traditional continuous barrier monitoring
#'   \item Discrete: Monitoring at specific intervals with Broadie-Glasserman-Kou adjustment
#'   \item Terminal: Barrier condition checked only at expiry
#' }
#'
#' @references
#' \itemize{
#'   \item Kirk, E. (1995) "Correlation in the Energy Markets." Managing Energy Price Risk,
#'         Risk Publications and Enron, London, pp. 71-78.
#'   \item Bjerksund, P. and Stensland, G. (2011) "Closed Form Spread Option Valuation."
#'         Quantitative Finance, Volume 14, Issue 10, pp. 1785-1794.
#'   \item Broadie, M., Glasserman, P., and Kou, S. (1997) "A Continuity Correction for
#'         Discrete Barrier Options." Mathematical Finance, 7, pp. 325-349.
#' }
#'
#' @examples
#' # Price a down-and-out call barrier spread option
#' F1 <- 100    # Forward price of first asset
#' F2 <- 110    # Forward price of second asset
#' X <- 5       # Strike price
#' B <- 95      # Barrier level
#' sigma1 <- 0.2   # Volatility of first asset
#' sigma2 <- 0.25  # Volatility of second asset
#' rho <- 0.5      # Correlation between assets
#' T2M <- 1        # Time to maturity (1 year)
#' r <- 0.05       # Risk-free rate
#'
#' # Price with continuous monitoring
#' result <- barrierSpreadOption(
#'   F1 = F1, F2 = F2, X = X, B = B,
#'   sigma1 = sigma1, sigma2 = sigma2,
#'   rho = rho, T2M = T2M, r = r,
#'   type = "call",
#'   barrier_type = "do",
#'   monitoring = "continuous"
#' )
#'
#' # Price with discrete (daily) monitoring
#' result_discrete <- barrierSpreadOption(
#'   F1 = F1, F2 = F2, X = X, B = B,
#'   sigma1 = sigma1, sigma2 = sigma2,
#'   rho = rho, T2M = T2M, r = r,
#'   type = "call",
#'   barrier_type = "do",
#'   monitoring = "discrete",
#'   monitoring_freq = 252
#' )
#'
#' @export barrierSpreadOption
barrierSpreadOption <- function(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                type = "call", barrier_type = "do",
                                monitoring = "continuous", monitoring_freq = 252) {
  # Input validation
  if (!is.numeric(F1) || !is.numeric(F2) || !is.numeric(X) || !is.numeric(B))
    stop("Asset prices, strike price, and barrier level must be numeric")
  if (!is.numeric(sigma1) || !is.numeric(sigma2) || sigma1 <= 0 || sigma2 <= 0)
    stop("Volatilities must be positive numbers")
  if (!is.numeric(rho) || abs(rho) > 1)
    stop("Correlation must be between -1 and 1")
  if (!is.numeric(T2M) || T2M <= 0)
    stop("Time to maturity must be positive")
  if (!is.numeric(r))
    stop("Interest rate must be numeric")
  if (!type %in% c("call", "put"))
    stop("Type must be 'call' or 'put'")
  if (!barrier_type %in% c("do", "di", "uo", "ui"))
    stop("Invalid barrier type. Must be 'do', 'di', 'uo', or 'ui'")
  if (!monitoring %in% c("continuous", "discrete", "terminal"))
    stop("Invalid monitoring approach")
  if (monitoring == "discrete" && (!is.numeric(monitoring_freq) || monitoring_freq <= 0))
    stop("Monitoring frequency must be a positive number for discrete monitoring")

  # Calculate option price
  option_price <- .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, r,
                                      type, barrier_type, monitoring, monitoring_freq)

  # Greeks calculations using numerical derivatives
  # Delta calculations
  delta_F1 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(x, F2, X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, F1)

  delta_F2 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, x, X, B, sigma1, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, F2)

  # Gamma calculations
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

  # Vega calculations
  vega_1 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, x, sigma2, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, sigma1)

  vega_2 <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, sigma1, x, rho, T2M, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, sigma2)

  # Theta calculation
  theta <- -numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, x, r,
                        type, barrier_type, monitoring, monitoring_freq)
  }, T2M)

  # Rho calculation
  rho_calc <- numDeriv::grad(function(x) {
    .barrierSpreadPrice(F1, F2, X, B, sigma1, sigma2, rho, T2M, x,
                        type, barrier_type, monitoring, monitoring_freq)
  }, r)

  # Return comprehensive results
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
    barrier_level = B,
    barrier_type = barrier_type,
    monitoring = monitoring
  ))
}
