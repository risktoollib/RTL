#' Commodity Swap details to learn their pricing
#' @description Returns dataframe required to price a WTI averaging instrument based on first line settlements.
#' @param date Character date as of which you want to extract daily settlement and forward values. `character`
#' @param feed Feeds for Morningstar getCurve() and getPrice(). `character`
#' @param exchange Exchange code in data(holidaysOil). Defaults to "nymex". `character`
#' @param ticker  Nymex contract code. `character`
#' @param contract Contract code in data(expiry_table). sort(unique(expiry_table$cmdty)) for options. `character`
#' @param iuser Morningstar user name as character - sourced locally in examples. `character`
#' @param ipassword Morningstar user password as character - sourced locally in examples. `character`
#' @param output "chart" or "all". `character`
#' @returns Plot or a list of data frame and plot if output = "all". `htmlwidget` or `list`
#' @export swapInfo
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' swapInfo(
#'   date = "2020-05-06", feed = "CME_NymexFutures_EOD_continuous",
#'   ticker = "CL",
#'   contract = "cmewti", exchange = "nymex",
#'   iuser = "x@xyz.com", ipassword = "pass", output = "all"
#' )
#' }
#'
swapInfo <- function(date = "2023-08-24",
                     feed = "CME_NymexFutures_EOD_continuous",
                     ticker = "CL",
                     contract = "cmewti", exchange = "nymex",
                     iuser = "x@xyz.com", ipassword = "pass", output = "all") {
  fig.title <- paste("Swap Pricing:", contract, "Futures Curve as of", date)

  wti <- getCurve(
    feed = feed, contract = ticker,
    date = date, fields = c("settlement_price"), iuser = iuser, ipassword = ipassword
  )

  to <- lubridate::rollforward(as.Date(date)) + months(4)
  calDays <- seq(as.Date(date), to = to, by = "day")
  hol <- RTL::holidaysOil %>% dplyr::filter(key == exchange)
  bizDays <- calDays[!(calDays %in% hol$value)]
  bizDays <- bizDays[!(weekdays(bizDays) %in% c("Saturday", "Sunday"))]

  curve <- dplyr::tibble(bizDays = bizDays) %>%
    dplyr::left_join(wti %>% dplyr::select(bizDays = expirationDate, fut.contract = code, price = Settlement), by = c("bizDays")) %>%
    tidyr::fill(price, fut.contract, .direction = c("up")) %>%
    tidyr::drop_na()

  hist <- RTL::getPrice(
    feed = feed, contract = paste0(ticker,"_001_MONTH"),
    from = as.character(lubridate::rollback(as.Date(date), roll_to_first = T)),
    iuser = iuser, ipassword = ipassword
  ) %>%
    dplyr::transmute(bizDays = date, fut.contract = "1stLineSettled", price = .[[2]])

  curve <- rbind(hist %>% dplyr::filter(bizDays <= date), curve %>% dplyr::slice(-1)) %>% stats::na.omit()

  chart <- curve %>%
    plotly::plot_ly(
      x = ~bizDays, y = ~price, type = "scatter", mode = "markers",
      color = ~fut.contract, text = ~fut.contract
    ) %>%
    plotly::layout(
      title = list(text = fig.title, x = 0),
      xaxis = list(title = paste(exchange, "Business Day")),
      yaxis = list(title = "$ per barrel", separators = ".,", tickformat = ".2f")
    )
  out <- list(curve, chart)
  if (output == "chart") {
    return(chart)
  } else {
    return(out)
  }
}
