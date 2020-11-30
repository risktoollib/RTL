#' \code{getGenscapeStorageOil}
#' @description
#' Returns oil storage data from Genscape API.You need your own credentials.
#' Refer to API documentation for argument values.
#' https://developer.genscape.com/docs/services/oil-storage/operations/StorageVolumeByOwnerGet
#'
#' @param feed "owner-volumes" DEFAULT or "tank-volumes"
#' @param regions See API webpage. Multiple values separated by commas e.g. "Canada, Cushing").
#' @param products See API webpage. Multiple values separated by commas e.g. "Crude, JetFuel").
#' @param revision See API webpage.
#' @param limit See API webpage. Max 5000
#' @param offset See API webpage.
#' @param startDate "yyyy-mm-dd" as character string
#' @param endDate "yyyy-mm-dd" as character string
#' @param apikey Your API key as a character string.
#' @return wide data frame
#' @export getGenscapeStorageOil
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getGenscapeStorageOil <- function(feed = "owner-volumes",regions = "Canada", products = "Crude",
#' evision = "revised", limit = 5000, offset = 0, startDate = "2011-01-01", endDate = "2020-11-01"
#' apikey = "<yourapikey>")
#' }

getGenscapeStorageOil <- function(feed = "owner-volumes",regions = "Canada", products = "Crude",
                        revision = "revised", limit = 5000, offset = 0,
                        startDate = "2011-01-01", endDate = as.character(Sys.Date()),
                        apikey = "yourapikey") {
  #https://api.genscape.com/storage/oil/v1/owner-volumes?regions=Canada&products=Crude&revision=revised&limit=5000&offset=0&format=json&genApiKey=
  url <- paste0("https://api.genscape.com/storage/oil/v1/",
                feed, "?",
                "regions=", gsub(" ","",regions),
                "&products=", gsub(" ","",products),
                "&revision=", revision,
                "&limit=", limit,
                "&offset=", offset,
                "&startDate=", startDate,
                "&endDate=", endDate,
                "&format=json",
                "&genApiKey=", apikey)

  out <- jsonlite::fromJSON(url)  %>% .[[1]] %>%
    dplyr::as_tibble() %>% dplyr::mutate(reportDate = as.Date(reportDate,"%Y-%m-%d"))

  minDate <- utils::tail(unique(out$reportDate),2)[1]

  while (minDate > startDate) {
    url <- paste0("https://api.genscape.com/storage/oil/v1/",
                  feed, "?",
                  "regions=", gsub(" ","",regions),
                  "&products=", gsub(" ","",products),
                  "&revision=", revision,
                  "&limit=", limit,
                  "&offset=", offset,
                  "&startDate=", startDate,
                  "&endDate=", minDate,
                  "&format=json",
                  "&genApiKey=", apikey)

    x <- jsonlite::fromJSON(url)  %>% .[[1]] %>%
      dplyr::as_tibble() %>% dplyr::mutate(reportDate = as.Date(reportDate,"%Y-%m-%d"))
    out <- rbind(out,x)
    if (length(unique(x$reportDate)) > 1) {
      minDate <- utils::tail(unique(out$reportDate),2)[1]
    } else { minDate <- startDate}
  }
  out <- unique(out)
  return(out)
}

#getGenscapeStorageOil(regions = "Canada, Cushing", products = "Crude",apikey = genscape$apikey)

