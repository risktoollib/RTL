#' Risk-reward statistics for quant trading
#' @description Compute list of risk reward metrics
#' @param x Univariate xts object of returns OR dataframe with date and return variables.
#' @param Rf Risk-free rate
#' @return List of risk/reward metrics.
#' @export tradeStats
#' @author Philippe Cote
#' @examples
#' library(PerformanceAnalytics)
#' x <- tidyquant::tq_get("SPY") %>% dplyr::mutate(ret = log(adjusted / dplyr::lag(adjusted)))
#' x <- x %>%
#'   stats::na.omit() %>%
#'   dplyr::select(date, ret)
#' tradeStats(x = x, Rf = 0)
tradeStats <- function(x, Rf = 0) {
  # if (!requireNamespace("PerformanceAnalytics", quietly = TRUE)) {
  #   stop("Package \"PerformanceAnalytics\" needed for this function to work. Please install it.",
  #        call. = FALSE)
  # }
  x <- x %>% stats::na.omit(x)
  if (class(x)[1] %in% c("xts", "zoo")) {
    x <- timetk::tk_tbl(x, rename_index = "date")
  }
  colnames(x) <- c("date", "ret")
  # cumret = as.numeric(PerformanceAnalytics::Return.cumulative(dplyr::select(x,ret),geometric = TRUE))
  cumret <- (cumprod(1 + x$ret) - 1) %>% dplyr::last()
  # ret.ann = as.numeric(PerformanceAnalytics::Return.annualized(x, scale = 252))
  perf <- tidyquant::tq_performance(x, Ra = ret, performance_fun = table.AnnualizedReturns)
  ret.ann <- perf$AnnualizedReturn
  # sd.ann = as.numeric(PerformanceAnalytics::sd.annualized(x, scale=252))
  sd.ann <- perf$AnnualizedStdDev
  downside <- tidyquant::tq_performance(x, Ra = ret, performance_fun = table.DownsideRiskRatio)
  # omega = as.numeric(PerformanceAnalytics::OmegaSharpeRatio(x,MAR = Rf) * sqrt(252))
  omega <- downside$`Omega-sharperatio`
  # sharpe = as.numeric(PerformanceAnalytics::SharpeRatio.annualized(x,Rf = Rf))
  sharpe <- perf$`AnnualizedSharpe(Rf=0%)`
  perc.win <- as.numeric(length(x$ret[x$ret > 0]) / length(x$ret[x$ret != 0]))
  perc.in.mkt <- as.numeric(length(x$ret[x$ret != 0]) / length(x$ret))
  y <- PerformanceAnalytics::findDrawdowns(timetk::tk_xts(x, date_var = date))
  DD.Length <- max(y$length)
  DD.Max <- min(y$return)
  out <- list(
    "CumReturn" = cumret,
    "Ret.Ann" = ret.ann,
    "SD.Ann" = sd.ann,
    "Sharpe" = sharpe,
    "Omega" = omega,
    "%.Win" = perc.win,
    "%.InMrkt" = perc.in.mkt,
    "DD.Length" = DD.Length,
    "DD.Max" = DD.Max
  )
  return(out)
}
