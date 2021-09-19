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
#' # where yourapikey = "yourapikey".
#' getGenscapeStorageOil(feed = "owner-volumes",regions = "Canada", products = "Crude",
#' revision = "revised", limit = 5000, offset = 0,
#' startDate = "2011-01-01", endDate = "2020-11-01", apikey = yourapikey)
#' }

getGenscapeStorageOil <- function(feed = "owner-volumes", regions = "Canada", products = "Crude",
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

#' \code{getGenscapePipeOil}
#' @description
#' Returns oil pipeline flows in barrels per day data from Genscape API.You need your own credentials.
#' Refer to API documentation for argument values. It is assumed if you use this function that
#' you know the pipelines you need to extract to build supply demand balances.
#' Use the online API to identify the pipeline IDs.
#' https://developer.genscape.com/docs/services/oil-transportation/operations/GetPipelineFlowValuesV2/
#'
#' @param frequency "daily" DEFAULT.
#' @param regions See API webpage. Multiple values separated by commas e.g. "Canada", "GulfCoast").
#' @param pipelineIDs See API webpage. c(98,54...) for specific pipes.
#' @param revision See API webpage.
#' @param limit See API webpage. Max 5000
#' @param offset See API webpage.
#' @param startDate "yyyy-mm-dd" as character string
#' @param endDate "yyyy-mm-dd" as character string
#' @param apikey Your API key as a character string.
#' @return wide data frame
#' @export getGenscapePipeOil
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getGenscapePipeOil(frequency = "daily", regions = "Canada",pipelineIDs = c(97),
#'                   revision = "revised", limit = 5000, offset = 0,
#'                   startDate = "2015-01-01", endDate = as.character(Sys.Date()),
#'                   apikey = "yourapikey")
#' }

getGenscapePipeOil <- function(frequency = "daily", regions = "Canada",pipelineIDs = c(97),
                                  revision = "revised", limit = 5000, offset = 0,
                                  startDate = "2015-01-01", endDate = as.character(Sys.Date()),
                                  apikey = "yourapikey") {
  #https://api.genscape.com/transportation/oil/v2/pipeline-flows/daily?regions=Canada&pipelineIDs=98,31&startDate=2015-01-01&limit=5000&offset=0&format=json&genApiKey=
  url <- paste0("https://api.genscape.com/transportation/oil/v2/pipeline-flows/",
                frequency,"?",
                paste0("pipelineIDs=",paste0(pipelineIDs,collapse = ","),"&"),
                "regions=", gsub(" ","",regions),
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

  while (minDate > startDate & nrow(out) > limit) {
    url <- paste0("https://api.genscape.com/transportation/oil/v2/pipeline-flows/",
                  frequency,"?",
                  if (pipelineIDs == "all") {""} else {paste0("pipelineIDs=",paste0(x,collapse = ","),"&")},
                  "regions=", gsub(" ","",regions),
                  "&revision=", revision,
                  "&limit=", limit,
                  "&offset=", offset,
                  "&startDate=", startDate,
                  #"&endDate=", minDate,
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


