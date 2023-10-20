#' Extract and convert GIS data from a URL
#' @description Returns a SpatialPointsDataFrame from a shapefile URL.
#'  @section Examples with EIA and Government of Alberta
#' \itemize{
#'   \item from https://www.eia.gov/maps/layer_info-m.php :
#'   \item crudepipelines <- getGIS(url = "https://www.eia.gov/maps/map_data/CrudeOil_Pipelines_US_EIA.zip")
#'   \item refineries <- getGIS(url = "https://www.eia.gov/maps/map_data/Petroleum_Refineries_US_EIA.zip")
#'   \item from https://gis.energy.gov.ab.ca/Geoview/OSPNG
#'   \item AB <- getGIS(url = "https://gis.energy.gov.ab.ca/GeoviewData/OS_Agreements_Shape.zip")
#' }
#' @param url URL of the zipped shapefile. `character`
#' @returns SpatialPointsDataFrame. `SpatialPolygonsDataFrame`
#' @export getGIS
#' @author Philippe Cote
#' @examples
#' \dontrun{
#' getGIS(url = "https://www.eia.gov/maps/map_data/CrudeOil_Pipelines_US_EIA.zip")
#' }
#'
getGIS <- function(url = "https://www.eia.gov/maps/map_data/CrudeOil_Pipelines_US_EIA.zip") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }


  # from the website download https://gis.energy.gov.ab.ca/Geoview/OSPNG
  td <- tempdir()
  tf <- tempfile(tmpdir = td, fileext = ".zip")
  utils::download.file(url, tf)
  fname <- utils::unzip(tf, list = TRUE)$Name
  utils::unzip(tf, files = fname, exdir = td, overwrite = TRUE)
  layer <- sub(".*/", "", sub(".shp.*", "", grep(".shp", fname, value = TRUE)[1]))
  dsn <- sub(paste0(layer, ".*"), "", file.path(td, fname)[1])
  out <-  sf::read_sf(dsn = dsn, layer = layer, quiet = TRUE)
  return(out)
}
