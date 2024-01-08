#' Cox-Ross-Rubinstein Option Pricing Model
#'
#' Computes the price of European and American options using the Cox-Ross-Rubinstein binomial model.
#' This function is optimized for performance and implemented in C++. Haug (2007) provides a detailed description of the model.
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
#' @examples
#' # CRROption(S = 100, X = 100, sigma = 0.25, r = 0.1, b = 0, T2M = 1, N = 500,
#' # type = "call", optionStyle = "european")
#' # CRROption(S = 100, X = 100, sigma = 0.25, r = 0.1, b = 0, T2M = 1, N = 500,
#' # type = "call", optionStyle = "american")
#'
#' @export
CRROption <- function(S, X, sigma, r, b, T2M, N, type, optionStyle) {
  if (sigma == 0) {sigma <- 10^-16}
  result <- CRROptionCpp(S, X, sigma, r, b, T2M, N, type, optionStyle)
  return(list(price = result$price, note = result$note))
}
