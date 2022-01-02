#' Commodity Calendar Month Average Swaps
#' @description Commodity swap pricing from exchange settlement
#' @param futures Wide data frame of futures prices for the given swap pricing dates
#' @param futuresNames Tickers of relevant futures contracts
#' @param pricingDates Vector of start and end pricing dates as character. See example.
#' @param exchange Exchange code in data(holidaysOil). Currently only "nymex" and "ice" supported.
#' @param contract Contract code in data(expiry_table). sort(unique(expiry_table$cmdty)) for options.
#' @return Data frame of histocial swap prices.
#' @export swapCOM
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' c <- paste0("CL0", c("M", "N", "Q"))
#' futs <- getPrices(
#'   feed = "CME_NymexFutures_EOD", contracts = c, from = "2019-08-26",
#'   iuser = username, ipassword = password
#' )
#' swapCOM(
#'   futures = futs, futuresNames = c("CL0M", "CL0N"),
#'   pricingDates = c("2020-05-01", "2020-05-30"), contract = "cmewti", exchange = "nymex"
#' )
#' }
#'
swapCOM <- function(futures = futs,
                    futuresNames = c("CL0M", "CL0N"),
                    pricingDates = c("2020-05-01", "2020-05-30"),
                    contract = "cmewti",
                    exchange = "nymex") {
  # Pricing days
  calDays <- seq(as.Date(pricingDates[1]), as.Date(pricingDates[2]), by = "day")
  hol <- RTL::holidaysOil %>% dplyr::filter(key == exchange)
  bizDays <- calDays[!(calDays %in% hol$value)]
  bizDays <- bizDays[!(weekdays(bizDays) %in% c("Saturday", "Sunday"))]

  # Expiries
  expiry <- RTL::expiry_table %>%
    dplyr::filter(
      cmdty == contract,
      Last.Trade >= pricingDates[1],
      Last.Trade <= pricingDates[2]
    ) %>%
    dplyr::select(Last.Trade)

  x <- dplyr::tibble(date = bizDays, Up2expiry = ifelse(date <= expiry$Last.Trade, 1, 0))
  first.fut.weight <- sum(x$Up2expiry) / nrow(x)

  # Compute Swap Price
  df <- futures %>%
    dplyr::select(date, futuresNames[1], futuresNames[2]) %>%
    dplyr::mutate(swap = .[[2]] * first.fut.weight + .[[3]] * (1 - first.fut.weight))
  return(df)
}
