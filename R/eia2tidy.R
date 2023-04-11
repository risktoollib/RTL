#' EIA API call with tidy output
#' @description Extracts data from the Energy Information Administration (EIA) API to tibble format with optional custom series name.
#' Makes a clean wrapper for use with purrr for multiple series extraction. Query Browser at https://www.eia.gov/opendata/qb.php.
#' @param ticker EIA series name. `character`
#' @param key Your private EIA API token as character "yourapikey". `character`
#' @param name Name you want to give the series. Defaults to ticker if set to " " `character`
#' @returns A tibble object with class date for weekly, monthly, quarterly or annual data and class POSIXct for hourly. `tibble`
#' @export eia2tidy
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' # Single Series
#' RTL::eia2tidy(ticker = "PET.MCRFPTX2.M", key = "yourapikey", name = "TexasProd")
#' # Multiple Series
#' # Use eia2tidy_all() or pivot_longer, drop_na and then pivot_wider to wrangled results.
#' }
eia2tidy <- function(ticker, key, name = " ") {
  #message("if using with multiple tickers use eia2tidy_all(). Code changed after the EIA API migrated to v2.")
  period <- NULL
  if (nchar(name) == 1) {
    name <- ticker
  }
  url <- paste0("https://api.eia.gov/v2/seriesid/",ticker,"?&api_key=",key)
  x <- url %>% httr::GET()
  if (x$status_code == "403") {stop("http 403 :: Invalid API key.")}
  if (x$status_code == "404") {stop("http 404 :: Ticker not found.")}
  if (x$status_code == "503") {stop("http 503 response :: the EIA server is temporarily unavailable. Try later.")}
  x <- jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"))
  #out <- x$response$data %>% dplyr::as_tibble() %>% dplyr::transmute(date = period, value) %>% dplyr::rename({{name}} := value)
  if (is.numeric(x$response$data$value) == FALSE) {x$response$data$value <- as.numeric(x$response$data$value)}

  out <- x$response$data %>% dplyr::as_tibble() %>%
    dplyr::select(date = period,tidyselect::where(is.numeric)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric),as.double))
  if (ncol(out) > 2) {out <- out %>% dplyr::select(date,dplyr::last(names(.)))}
  names(out)[ncol(out)] <- name
  freq <- x$response$frequency
  tmp <- out$date
  if (freq == "monthly") {out$date <- as.Date(paste(substr(tmp, 1, 4), substr(tmp, 6, 7), "01", sep = "-"), format = "%Y-%m-%d")} # working PET.MCRFPTX2.M
  if (freq == "annual") {out$date <- as.Date(paste0(tmp, "-01-01"), format = "%Y-%m-%d")} # working PET.MCRFPTX2.A
  if (freq == "quarterly") {out$date <- zoo::as.Date(zoo::as.yearqtr(tmp, format = "%Y-Q%q"), frac = 1)} # ELEC.PLANT.CONS_EG_BTU.2522-ALL-ALL.Q
  if (freq %in% c("daily","weekly")) {out$date <- as.Date(tmp, format = "%Y-%m-%d")} # working PET.WCREXUS2.W
  if (freq == "hourly") {out$date <- lubridate::parse_date_time(tmp, c("'%Y%m%d%h"))}

  return(out)
}
