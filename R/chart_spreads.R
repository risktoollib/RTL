#' \code{chart_spreads}
#' @description Chart spreads in specific futures contracts for multiple years.
#' @param cpairs Data frame of contract pairs - see example.
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
#' cpairs <- dplyr::tibble(year = c("2014","2019","2020"),
#' first = c("@HO4H","@HO9H","@HO0H"),
#' second = c("@CL4J","@CL9J","@CL0J"))
#' chart_spreads(cpairs = cpairs, daysFromExpiry = 200, from = "2012-01-01",
#' conversion = c(42,1),feed = "CME_NymexFutures_EOD",
#' iuser = "x@xyz.com", ipassword = "pass",
#' title = "March/April ULSD Nymex Spreads",
#' yaxis = "$ per bbl",
#' output = "data")
#'  }

chart_spreads <- function(cpairs = cpairs,
                          daysFromExpiry = 200,
                          from = "2012-01-01",
                          conversion = c(1,1),
                          feed = "CME_NymexFutures_EOD",
                          iuser = "x@xyz.com",
                          ipassword = "pass",
                          title = "March/April ULSD Nymex Spreads",
                          yaxis = "$ per bbl",
                          output = "chart"){

  out <- dplyr::tibble(year = NA, value = NA, DaysFromExp = NA, date = Sys.Date())

  for (i in 1:nrow(cpairs)) {

    x <- RTL::getPrices(feed = feed,
                        contracts = c(dplyr::pull(cpairs,var = c("first"))[i],
                                      dplyr::pull(cpairs,var = c("second"))[i]),
                        from = from, iuser = iuser, ipassword = ipassword) %>%
      dplyr::transmute(year = !!dplyr::pull(cpairs,var = c("year"))[i],
                       value := !!dplyr::sym(gsub("@","",dplyr::pull(cpairs,var = c("first"))[i])) * conversion[1] -
                         !!dplyr::sym(gsub("@","",dplyr::pull(cpairs,var = c("second"))[i])) * conversion[2],
                       DaysFromExp = -seq(nrow(.),1,-1),
                       date = date) %>%
      dplyr::filter(DaysFromExp >= -daysFromExpiry)
    out <- rbind(out,x)
  }

  out <- out %>% stats::na.omit()

  if (output != "chart") {return(out)} else {
  out <- out %>%
    plotly::plot_ly(x = ~DaysFromExp, y = ~value, name = ~year, color = ~year) %>%
    plotly::add_lines() %>%
    plotly::layout(title = list(text = title, x = 0),
                   yaxis = list(title = yaxis, tickformat = ".2f"))
  return(out)
  }
}


