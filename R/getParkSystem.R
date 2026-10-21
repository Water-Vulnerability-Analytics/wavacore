#' @title Download the boundaries of all NPS units
#'
#' @description
#' Downloads the boundaries of every NPS unit from the NPS Data Store as a
#' zipped shapefile (`nps_boundary.shp`, about 29 MB), reads it, and repairs
#' invalid geometries with [sf::st_make_valid()]. Needs internet.
#'
#' The download link is fixed in the code (last updated September 2025). To
#' find a newer one, see the Data Store reference page
#' <https://irma.nps.gov/DataStore/Reference/Profile/2224545> and the Data
#' Store API documentation
#' <https://irmaservices.nps.gov/datastore/v6/documentation/datastore-api.html>.
#'
#' @param save Logical. If `TRUE` and `path` is given, also writes the
#'   boundaries to a GeoPackage named `nps_park_boundaries_<date>.gpkg` in
#'   `path`. Default `FALSE`.
#' @param path Folder to write the GeoPackage to when `save = TRUE`. If `NULL`
#'   (default), nothing is written, even with `save = TRUE`.
#'
#' @return An sf object of multipolygons (CRS NAD83), one row per NPS unit,
#'   with columns such as `UNIT_CODE`, `UNIT_NAME`, `UNIT_TYPE`, `STATE` and
#'   `REGION`.
#' @export
#' @seealso [getParkBoundary()] for selected units only; [get_wsd()], which
#'   uses these boundaries to fill in missing coordinates.
#' @examplesIf interactive()
#' parks <- getParkSystem()
#' parks[parks$UNIT_TYPE == "National Park", c("UNIT_CODE", "UNIT_NAME")]
#'
#' # Also save a copy as a GeoPackage
#' parks <- getParkSystem(save = TRUE, path = tempdir())
#' list.files(tempdir(), pattern = "gpkg$")

getParkSystem <- function(save = FALSE, path = NULL){
  

  downloadLink <-  "https://irma.nps.gov/DataStore/DownloadFile/733895"
  
  #download boundary 
  temp1 <- tempfile()
  utils::download.file(downloadLink, destfile = temp1, method = "curl")
  temp2 <- tempfile()
  utils::unzip(temp1, exdir = temp2)
  
  
  parks <- sf::st_read(paste0(temp2, "/nps_boundary.shp")) %>%
    sf::st_make_valid()
  
  # Optionally save the data
  if (save && !is.null(path)) {
    sf::st_write(parks, paste0(path, "/", "nps_park_boundaries_", Sys.Date(), ".gpkg"), delete_dsn = TRUE)
  }
  
  
  return(parks)
  
}

