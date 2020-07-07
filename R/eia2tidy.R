#' \code{eia2tidy}
#' @description Extracts data from the Energy Information Administration (EIA) API to tibble format with optional custom series name.
#' Makes a clean wrapper for use with purrr for multiple series extraction.
#' @param ticker EIA series name.
#' @param key Your private EIA API token.
#' @param name Name you want to give the series. Defaults to ticker if set to " "
#' @return A tibble object
#' @export eia2tidy
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' eia_df <-tibble::tribble(~ticker, ~name,
#' "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
#' "NG.NW2_EPG0_SWO_R48_BCF.W","NGLower48") %>%
#'  dplyr::mutate(key = EIAkey) %>%
#'  dplyr::mutate(df = purrr::pmap(list(ticker,key,name),.f=RTL::eia2tidy)) %>%
#'  dplyr::select(df) %>% tidyr::unnest(df)
#'  }

eia2tidy <- function(ticker,key,name = " ") {
  #x <- EIAdata::getEIA(ID=ticker,key=key)
  #names(x) <- "value"
  #out <- x %>% timetk::tk_tbl(rename_index = "date")
  if (nchar(name)==1) {name <- ticker}
  url <-  paste0("http://api.eia.gov/series/?api_key=",key,"&series_id=",ticker,"&out=json")
  x <- url %>% httr::GET()
  x <- jsonlite::fromJSON(httr::content(x,"text",encoding = "UTF-8"))
  out <- x$series$data[[1]] %>%
    dplyr::as_tibble(.name_repair = ~ c("date","value")) %>%
    dplyr::mutate(series = name,
                  date = dplyr::case_when(nchar(date)==4 ~as.Date(paste0(date,"-01-01"),format="%Y-%m-%d"),
                                 nchar(date)==6 ~as.Date(paste(substr(date,1,4),substr(date,5,6),"01",sep="-"),format="%Y-%m-%d"),
                                 nchar(date)==8 ~as.Date(date,format="%Y%m%d")),
                  value = as.numeric(value)) %>%
    dplyr::select(series,date,value)
  return(out)
}
