#' Commodity Calendar Month Average Swap futures weights
#' @description Returns the percentage weight of the future in Calendar Month Average swaps
#' @param Month First calendar day of the month.
#' @param exchange Exchange code in data(holidaysOil). Currently only "nymex" and "ice" supported.
#' @param contract Contract code in data(expiry_table). sort(unique(expiry_table$cmdty)) for options.
#' @param output Either "numDaysFut1", "numDaysFut2" or "first.fut.weight"
#' @return What you defined in outputs.
#' If first.fut.weight, to compute swap 1 - first.fut.weight = % applied to 2nd line contract.
#' @export swapFutWeight
#' @author Philippe Cote
#' @examples
#' swapFutWeight(
#'   Month = "2020-09-01",
#'   contract = "cmewti", exchange = "nymex", output = "first.fut.weight"
#' )
swapFutWeight <- function(Month = "2020-09-01",
                          contract = "cmewti",
                          exchange = "nymex",
                          output = "first.fut.weight") {
  # Pricing days
  m <- as.Date(Month)
  m1 <- lubridate::rollback(m + months(1))
  calDays <- seq(as.Date(m), m1, by = "day")
  hol <- RTL::holidaysOil %>% dplyr::filter(key == exchange)
  bizDays <- calDays[!(calDays %in% hol$value)]
  bizDays <- bizDays[!(weekdays(bizDays) %in% c("Saturday", "Sunday"))]

  # Expiries
  expiry <- RTL::expiry_table %>%
    dplyr::filter(
      cmdty == contract,
      Last.Trade >= m,
      Last.Trade <= m1
    ) %>%
    dplyr::select(Last.Trade)

  x <- dplyr::tibble(date = bizDays, Up2expiry = ifelse(date <= expiry$Last.Trade, 1, 0))
  numDaysFut1 <- sum(x$Up2expiry)
  numDaysFut2 <- nrow(x) - sum(x$Up2expiry)
  first.fut.weight <- sum(x$Up2expiry) / nrow(x)

  if (output == "numDaysFut1") {
    out <- numDaysFut1
  }
  if (output == "numDaysFut2") {
    out <- numDaysFut2
  }
  if (output == "first.fut.weight") {
    out <- first.fut.weight
  }

  # out <- list(numDaysFut1 = sum(x$Up2expiry),
  #             numDaysFut2 = nrow(x) - sum(x$Up2expiry),
  #             first.fut.weight)
  return(out)
}
