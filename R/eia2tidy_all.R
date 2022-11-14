#' EIA API multiple calls with tidy output
#' @description Extracts data from the Energy Information Administration (EIA) API to tibble format with optional custom series name.
#' Makes a clean wrapper for use with purrr for multiple series extraction. Query Browser at https://www.eia.gov/opendata/qb.php.
#' @param tickers tribble of EIA series and names you want to assign.
#' @param key Your private EIA API token as character "yourapikey".
#' @param long TRUE (default) to return a long data frame or FASLE for wide
#' @return A tibble object with class date for weekly, monthly, quarterly or annual data and class POSIXct for hourly.
#' @export eia2tidy_all
#' @author Philippe Cote
#' @examples
#' \dontrun{
#'eia2tidy_all(tickers = tibble::tribble(~ticker, ~name,
#'                          "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
#'                          "NG.NW2_EPG0_SWO_R48_BCF.W", "NGLower48"),
#'             key = "your API key", long = TRUE)
#' }
eia2tidy_all <- function(tickers = tibble::tribble(~ticker, ~name,
                                               "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
                                               "NG.NW2_EPG0_SWO_R48_BCF.W", "NGLower48"),
                     key,
                     long = TRUE) {
  out <- tickers %>%
    dplyr::mutate(key = key) %>%
    dplyr::mutate(df = purrr::pmap(list(ticker, key, name), .f = RTL::eia2tidy)) %>%
    dplyr::select(df) %>%
    tidyr::unnest(df) %>%
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
    tidyr::drop_na()
  if (long == FALSE) {out <-out %>% tidyr::pivot_wider(names_from = "series", values_from = "value")}
  return(out)
}
