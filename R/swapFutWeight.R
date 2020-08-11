#' \code{swapFutWeight}
#' @description Returns the percentage weight of the future in Calendar Month Average swaps
#' @param Month First calendar day of the month.
#' @param exchange Exchange code in data(holidaysOil). Currently only "nymex" and "ice" supported.
#' @param contract Contract code in data(expiry_table). sort(unique(expiry_table$cmdty)) for options.
#' @return Percentage applied to first contract. To compute swap 1 - value = % applied to 2nd line contract.
#' @export swapFutWeight
#' @author Philippe Cote
#' @examples
#' swapFutWeight(Month = "2020-09-01",contract = "cmewti",exchange = "nymex")

swapFutWeight <- function(Month = "2020-09-01",
                          contract = "cmewti",
                          exchange = "nymex") {
  # Pricing days
  m = as.Date(Month)
  m1 = lubridate::rollback(m + months(1))
  calDays <- seq(as.Date(m),m1,by="day")
  hol <- holidaysOil %>% dplyr::filter(key == exchange)
  bizDays <- calDays[!(calDays %in% hol$value)]
  bizDays <- bizDays[!(weekdays(bizDays) %in% c('Saturday','Sunday'))]

  # Expiries
  expiry <- expiry_table %>%
    dplyr::filter(cmdty == contract,
                  Last.Trade >= m,
                  Last.Trade <= m1) %>%
    dplyr::select(Last.Trade)

  x <- dplyr::tibble(date = bizDays,Up2expiry = ifelse(date <= expiry$Last.Trade,1,0))
  first.fut.weight = sum(x$Up2expiry) / nrow(x)
  return(first.fut.weight)
}
