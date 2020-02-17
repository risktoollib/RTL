#' \code{eia2tidy}
#' @description Converts output of getEAI() in a tidy tibble with names("date","value"). Makes a clean wrapper for use with purrr.
#' @param ticker EIA series name.
#' @param key EIA API token.
#' @return A tibble object
#' @export eia2tidy
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' eia_df <-tibble::tribble(~ticker, ~series,
#' "PET.W_EPC0_SAX_YCUOK_MBBL.W", "Cushing Crude Stocks",
#' "NG.NW2_EPG0_SWO_R48_BCF.W","NG Storage - Lower 48") %>%
#'  dplyr::mutate(key = EIAkey) %>%
#'  dplyr::mutate(df = purrr::pmap(list(ticker,key),.f=eia2tidy)) %>%
#'  dplyr::select(series, df) %>% tidyr::unnest()
#'  }

eia2tidy <- function(ticker,key) {
  x <- EIAdata::getEIA(ID=ticker,key=key)
  names(x) <- "value"
  out <- x %>% timetk::tk_tbl(rename_index = "date")
}
