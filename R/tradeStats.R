#' \code{tradeStats}
#' @description Compute list of risk reward metrics
#' @param x xts object of returns
#' @param Rf Risk-free rate
#' @return List of risk/reward metrics.
#' @export tradeStats
#' @author Philippe Cote
#' @examples
#' library(quantmod)
#' getSymbols("SPY", return.class = "zoo")
#' SPY$retClCl <- na.omit(quantmod::Delt(Cl(SPY),k=1,type='arithmetic'))
#' tradeStats(x=SPY$retClCl,Rf=0)
tradeStats <- function(x,Rf=0) {
  x = stats::na.omit(x)
  cumret = as.numeric(PerformanceAnalytics::Return.cumulative(x,geometric = TRUE))
  ret.ann = as.numeric(PerformanceAnalytics::Return.annualized(x, scale=252))
  sd.ann = as.numeric(PerformanceAnalytics::sd.annualized(x, scale=252))
  omega = as.numeric(PerformanceAnalytics::OmegaSharpeRatio(x,MAR = Rf)*sqrt(252))
  sharpe = as.numeric(PerformanceAnalytics::SharpeRatio.annualized(x,Rf = Rf))
  perc.win = as.numeric(length(x[x > 0])/length(x[x != 0]))
  perc.in.mkt = as.numeric(length(x[x != 0])/length(x))
  y = PerformanceAnalytics::findDrawdowns(x)
  DD.Length = max(y$length)
  DD.Max = min(y$return)
  out = list("CumReturn"=cumret,
             "Ret.Ann"=ret.ann, "SD.Ann"=sd.ann,
             "Sharpe"=sharpe[1], "Omega"=omega,
             "%.Win"=perc.win,
             "%.InMrkt"=perc.in.mkt,
             "DD.Length"=DD.Length,"DD.Max"=DD.Max)
  return(out)
}
