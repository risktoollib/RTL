#' Futures contract spreads comparison across years
#' @description Plots specific contract pairs across years with time being days from expiry.
#' @param cpairs Tibble of contract pairs - see example for expiry when not expired yet.
#' @param daysFromExpiry Number of days (numeric) from expiry to compute spreads.
#' @param conversion Defaults to c(1,1) first and second contracts. 42 from $ per gallons to bbls.
#' @param feed Morningstar Feed Table.
#' @param from From date as character string
#' @param iuser Morningstar user name as character - sourced locally in examples.
#' @param ipassword Morningstar user password as character - sourced locally in examples.
#' @param title Title for chart.
#' @param yaxis y-axis label.
#' @param output "chart" for plotly object or "data" for dataframe.
#' @return A plotly object or a dataframe
#' @importFrom rlang :=
#' @export chart_spreads
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' cpairs <- dplyr::tibble(
#'  year = c("2018", "2019", "2020","2021","2022","2023"),
#'  first = c("@HO8H", "@HO9H", "@HO0H","@HO21H","@HO22H","@HO23H"),
#'  second = c("@CL8H", "@CL9H", "@CL0H","@CL21H","@CL22H","@CL23H"),
#'  expiry = c(NA,NA,NA,NA,NA,"2023-02-23")
#')
#' chart_spreads(
#'   cpairs = cpairs, daysFromExpiry = 200, from = "2012-01-01",
#'   conversion = c(42, 1), feed = "CME_NymexFutures_EOD",
#'   iuser = "x@xyz.com", ipassword = "pass",
#'   title = "March/April ULSD Nymex Spreads",
#'   yaxis = "$ per bbl",
#'   output = "data"
#' )
#' }
#'
chart_spreads <- function(cpairs = cpairs,
                          daysFromExpiry = 200,
                          from = "2012-01-01",
                          conversion = c(1, 1),
                          feed = "CME_NymexFutures_EOD",
                          iuser = "x@xyz.com",
                          ipassword = "pass",
                          title = "March/April ULSD Nymex Spreads",
                          yaxis = "$ per bbl",
                          output = "chart") {
  out <- dplyr::tibble(year = NA, value = NA, DaysFromExp = NA, date = Sys.Date())

  for (i in 1:nrow(cpairs)) {
    x <- RTL::getPrices(
      feed = feed,
      contracts = c(
        dplyr::pull(cpairs, var = c("first"))[i],
        dplyr::pull(cpairs, var = c("second"))[i]
      ),
      from = from, iuser = iuser, ipassword = ipassword
    ) %>%
      dplyr::transmute(
        year = !!dplyr::pull(cpairs, var = c("year"))[i],
        value := !!dplyr::sym(gsub("@", "", dplyr::pull(cpairs, var = c("first"))[i])) * conversion[1] -
          !!dplyr::sym(gsub("@", "", dplyr::pull(cpairs, var = c("second"))[i])) * conversion[2],
        DaysFromExp = -seq(nrow(.), 1, -1),
        date = date
      )
    tmm <- NULL
    tmp <- dplyr::pull(cpairs, var = c("expiry"))[i]

    if (!is.na(tmp)) {
      calDays <- seq(as.Date(x %>% dplyr::filter(date == dplyr::last(date)) %>% dplyr::pull()),
                             as.Date(as.Date(tmp)), by = "day")
      hol <- RTL::holidaysOil %>% dplyr::filter(key == "cmewti")
      bizDays <- calDays[!(calDays %in% hol$value)]
      bizDays <- bizDays[!(weekdays(bizDays) %in% c("Saturday", "Sunday"))] %>% length(.)
      tmp <- -seq(nrow(x)+bizDays-1, 1, -1)[1:nrow(x)]
      x <- x %>% dplyr::mutate(DaysFromExp = tmp)
    }

    x <- x %>%
      dplyr::filter(DaysFromExp >= -daysFromExpiry)

    out <- rbind(out, x)
  }

  out <- out %>% stats::na.omit()

  if (output != "chart") {
    return(out)
  } else {
    out <- out %>%
      plotly::plot_ly(x = ~DaysFromExp, y = ~value, name = ~year, color = ~year) %>%
      plotly::add_lines() %>%
      plotly::layout(
        title = list(text = title, x = 0),
        yaxis = list(title = yaxis, tickformat = ".2f")
      )
    return(out)
  }
}
