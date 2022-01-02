#' EIA API call with tidy output
#' @description Extracts data from the Energy Information Administration (EIA) API to tibble format with optional custom series name.
#' Makes a clean wrapper for use with purrr for multiple series extraction. Query Browser at https://www.eia.gov/opendata/qb.php.
#' @param ticker EIA series name.
#' @param key Your private EIA API token as character "<yourapikey>".
#' @param name Name you want to give the series. Defaults to ticker if set to " "
#' @return A tibble object with class date for weekly, monthly, quarterly or annual data and class POSIXct for hourly.
#' @export eia2tidy
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' # Single Series
#' RTL::eia2tidy(ticker = "PET.MCRFPTX2.M", key = "yourapikey", name = "TexasProd")
#' # Multiple Series
#' eia_df <- tibble::tribble(
#'   ~ticker, ~name,
#'   "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
#'   "NG.NW2_EPG0_SWO_R48_BCF.W", "NGLower48"
#' ) %>%
#'   dplyr::mutate(key = "EIAkey") %>%
#'   dplyr::mutate(df = purrr::pmap(list(ticker, key, name), .f = RTL::eia2tidy)) %>%
#'   dplyr::select(df) %>%
#'   tidyr::unnest(df)
#' }
eia2tidy <- function(ticker, key, name = " ") {
  if (nchar(name) == 1) {
    name <- ticker
  }
  url <- paste0("http://api.eia.gov/series/?api_key=", key, "&series_id=", ticker, "&out=json")
  x <- url %>% httr::GET()
  x <- jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"))
  # promises::promise(~print(x$data$error)) %>%
  #   promises::then()
  # x %>% promises::promise()
  out <- x$series$data[[1]] %>% dplyr::as_tibble(.name_repair = ~ c("date", "value"))

  if (nchar(out$date)[1] == 12) {
    out <- out %>%
      dplyr::mutate(
        series = name,
        date = lubridate::parse_date_time(date, c("'%Y%m%d%h")),
        value = as.numeric(value)
      ) %>%
      dplyr::select(series, date, value)
  } else {
    out <- out %>%
      dplyr::mutate(
        series = name,
        date = dplyr::case_when(
          nchar(date) == 4 ~ as.Date(paste0(date, "-01-01"), format = "%Y-%m-%d"),
          grepl("Q", date) ~ zoo::as.Date(zoo::as.yearqtr(date, format = "%YQ%q"), frac = 1),
          nchar(date) == 6 ~ as.Date(paste(substr(date, 1, 4), substr(date, 5, 6), "01", sep = "-"), format = "%Y-%m-%d"),
          nchar(date) == 8 ~ as.Date(date, format = "%Y%m%d")
        ),
        value = as.numeric(value)
      ) %>%
      dplyr::select(series, date, value)
  }
  return(out)
}
