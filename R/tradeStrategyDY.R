#' Sample quantitative trading strategy
#' @description Based on dividend yield
#' @param data Dataframe of OHLC data e.g. RTL::uso
#' @param par1value Value of first parameter e.g. short MA
#' @param par2value Value of second parameter e.g. long MA
#' @return Dataframe with indicators, signals, trades and profit and loss.
#' @export tradeStrategyDY
#' @author Philippe Cote
#' @examples
#' tradeStrategyDY(data = RTL::stocks$ry, par1value = 50, par2value = 200)
tradeStrategyDY <- function(data,
                     par1value = 50,
                     par2value = 200) {
  Open <- trade <- pos <- retOpCl <- retClCl <- retClCl <- ret_new <- ret_exist <- ret_others <- NULL

  data <- data %>%
    dplyr::mutate(
      # returns
      retClCl = Close / dplyr::lag(Close) - 1,
      retOpCl = (Close - Open) / Close,
      retClOp = Open / dplyr::lag(Close) - 1,
      # Indicators
      par1 = TTR::SMA(Close, n = par1value),
      par2 = TTR::SMA(Close, n = par2value),
      # Signals
      signal = dplyr::case_when(par1 > par2 ~ 1,
                                par1 < par2 ~ -1,
                                TRUE ~ 0),
      # Trade
      trade = tidyr::replace_na(dplyr::lag(signal) - dplyr::lag(signal, n =
                                                                  2L), 0),
      # Positions
      pos = cumsum(trade),
      # PL
      ret_new = ifelse(pos == trade , pos * retOpCl, 0),
      ret_exist = ifelse(pos != 0 & trade == 0, pos * retClCl, 0),
      ret_others = dplyr::case_when((pos - trade) != 0 &
                                      trade != 0 ~ (1 + retClOp * (pos - trade)) * (1 + retOpCl * pos) - 1,
                                    TRUE ~ 0
      ),
      ret = ret_new + ret_exist + ret_others,
      # Compute Cumulative Equity or PL
      cumeq = cumprod(1 + ret)
    )
  return(data)
}
