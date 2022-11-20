#' Bank of Canada Valet API
#' @description Extracts series from the Bank of Canada's Valet API.
#' API documentation at https://www.bankofcanada.ca/valet/docs.
#' @param series Array of series name: c("FXCADUSD","BD.CDN.2YR.DQ.YLD").
#' @return A long data frame.
#' @export getBoC
#' @author Philippe Cote
#' @examples
#' RTL::getBoC(series = c("FXCADUSD","BD.CDN.2YR.DQ.YLD"))

getBoC <- function(series) {
  for (s in series) {
    x <- httr::GET(url = paste0("https://www.bankofcanada.ca/valet/observations/",s,"/json"))
    if (x$status_code == "404") {stop(print("http 404 :: Series not found."))}
    if (x$status_code == "503") {stop(print("http 503 :: the BoC server is temporarily unavailable."))}
    tmp <- jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"))
    tmp <- dplyr::tibble(date = as.Date(tmp$observations$d), {{s}} := as.numeric(tmp$observations[[s]]$v)) %>%
      tidyr::pivot_longer(-date, names_to = "series",values_to = "value")

  if (s == series[[1]]) {out <- tmp} else {
    out <- rbind(out, tmp)
    }
  }

  return(out)
}
