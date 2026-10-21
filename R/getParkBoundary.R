#' @title Get the boundaries of selected NPS units
#'
#' @description
#' Downloads the boundaries of all NPS units with [getParkSystem()], keeps the
#' units in `park`, and merges each unit into a single (multi)polygon.
#' Optionally saves one shapefile per unit. Needs internet.
#'
#' @param park Character vector of four-letter unit codes (`UNIT_CODE`), e.g.
#'   `c("ROMO", "GRCA")`. Codes that match no unit are dropped without a
#'   warning.
#' @param save Logical. If `TRUE`, writes each unit to
#'   `<path><code>/<code>_park_boundary.shp`, creating the `<code>` folder if
#'   needed. Default `FALSE`.
#' @param path Folder to save the shapefiles in when `save = TRUE`. It must
#'   end with `/`.
#'
#' @return An sf object (CRS NAD83) with one row per unit and columns
#'   `UNIT_CODE`, `REGION`, `STATE`, `UNIT_NAME`, `PARKNAME` and `geometry`,
#'   grouped by all of them except `geometry`.
#' @export
#' @seealso [getParkSystem()] for every unit.
#' @examplesIf interactive()
#' bounds <- getParkBoundary(c("ROMO", "GRCA"))
#' plot(sf::st_geometry(bounds))
#'
#' # Also save one shapefile per park in tempdir()
#' bounds <- getParkBoundary("ROMO", save = TRUE, path = paste0(tempdir(), "/"))
#' list.files(file.path(tempdir(), "ROMO"))
getParkBoundary <- function(park, save = FALSE, path = NULL){
  
  parks <- getParkSystem()
  
  park_boundary <- parks %>% 
    sf::st_make_valid() %>%
    dplyr::filter(UNIT_CODE %in% park) %>%
    dplyr::group_by(UNIT_CODE,REGION,STATE,UNIT_NAME,PARKNAME) %>%
    dplyr::summarize(.groups = "keep")
  
  if(save == TRUE){
    
    for (i in 1:length(park)){
      
      #create park folder if does not already exist in 'path'
      if (!dir.exists(paste0(path, park[i]))) {
        
        dir.create(paste0(path, park[i]))
      }
      
      # save park boundary in park folder
      sf::write_sf(dplyr::filter(park_boundary, UNIT_CODE == park[i]), 
                   paste0(path, "/", park[i], "/", park[i], "_park_boundary.shp"))
      
      
    }
    
    print(paste("Park boundary shapefiles saved to respective folders in:", paste0(getwd(), "/", path)))
    
  }
  
  return(park_boundary)
  
}