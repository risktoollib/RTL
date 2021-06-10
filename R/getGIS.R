#' \code{getGIS}
#' @description Returns a SpatialPointsDataFrame from a shapefile URL.
#'  @section Examples with EIA and Government of Alberta
#' \itemize{
#'   \item from https://www.eia.gov/maps/layer_info-m.php :
#'   \item crudepipelines <- getGIS(url = "https://www.eia.gov/maps/map_data/CrudeOil_Pipelines_US_EIA.zip")
#'   \item refineries <- getGIS(url = "https://www.eia.gov/maps/map_data/Petroleum_Refineries_US_EIA.zip")
#'   \item from https://gis.energy.gov.ab.ca/Geoview/OSPNG
#'   \item AB <- getGIS(url = "https://gis.energy.gov.ab.ca/GeoviewData/OS_Agreements_Shape.zip")
#' }
#' @param url URL of the zipped shapefile
#' @return SpatialPointsDataFrame
#' @export getGIS
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getGIS(url = url = "https://gis.energy.gov.ab.ca/GeoviewData/OS_Agreements_Shape.zip")
#' }

getGIS <- function(url = "https://gis.energy.gov.ab.ca/GeoviewData/OS_Agreements_Shape.zip") {

  if (!requireNamespace("rgdal", quietly = TRUE)) {
    stop("Package \"rgdal\" needed for this function to work. Please install it.",
         call. = FALSE)
  }


  # from the website download https://gis.energy.gov.ab.ca/Geoview/OSPNG
  td <- tempdir()
  tf <- tempfile(tmpdir = td,fileext = ".zip")
  utils::download.file(url,tf)
  fname <- utils::unzip(tf,list = TRUE)$Name
  utils::unzip(tf,files = fname,exdir = td,overwrite = TRUE)
  layer <-  sub(".*/","",sub(".shp.*","",grep(".shp",fname,value = TRUE)[1]))
  dsn <- sub(paste0(layer,".*"),"",file.path(td,fname)[1])
  out <- rgdal::readOGR(dsn = dsn, layer = layer)
  return(out)
}



