#' @title Load the NPS Water Supply Database (WSD)
#'
#' @description
#' Reads the NPS Water Supply Database workbook and cleans the column names.
#' Optionally fills in missing source coordinates, keeps only the contiguous
#' US or some park units, and converts the result to an sf object of points.
#' Stops with an error if the workbook is not found at `path`.
#'
#' @param conus Logical. Whether to keep only sources in the contiguous US.
#'   * `TRUE`: drops sources whose `state` is AK, AS, HI, MP, PR, VI or GU
#'     (Alaska, Hawaii and the territories).
#'   * `FALSE`: keeps sources in every state and territory.
#'
#'   Ignored when `rapid = TRUE`, which always uses `TRUE`. Default: `TRUE`.
#' @param fill_empty_coords Logical. Whether to fill in coordinates for sources
#'   that have no longitude or latitude in the database.
#'   * `TRUE`: downloads the NPS unit boundaries ([getParkSystem()]) and the US
#'     state boundaries, so it needs internet and takes longer. Each source
#'     with missing coordinates gets a point inside its park unit. For a unit
#'     made of separate pieces, the point is in the piece in the source's
#'     state (or the park's state when `source_state` is empty); if several
#'     pieces match, the first one is used. Sources whose park unit has no
#'     boundary keep empty coordinates. Adds a `coords_flag` column
#'     (`"Source Coordinates"` or `"Centroid"`), and the filled rows are moved
#'     to the end of the table.
#'   * `FALSE`: keeps the coordinates as they are in the database, downloads
#'     nothing, and adds no `coords_flag` column.
#'
#'   Ignored when `rapid = TRUE`, which always uses `TRUE`. Default: `FALSE`.
#' @param park Character vector or `NULL`. The park units to keep.
#'   * A character vector of four-letter unit codes, e.g. `c("ROMO", "GRCA")`:
#'     keeps only sources whose `park_unit` is one of them. Codes that match
#'     nothing are dropped without a warning. The filter runs after the
#'     coordinate fill, so it doesn't make `fill_empty_coords = TRUE` faster.
#'   * `NULL`: keeps every park.
#'
#'   Default: `NULL`.
#' @param sf Logical. The class of the result.
#'   * `TRUE`: returns an sf object of points (CRS 4326, longitude/latitude)
#'     built from `source_longitude` and `source_latitude`, which are kept as
#'     columns. Sources that still have no coordinates are dropped.
#'   * `FALSE`: returns a tibble with every source, including those with no
#'     coordinates.
#'
#'   Ignored when `rapid = TRUE`, which always uses `TRUE`. Default: `TRUE`.
#' @param rapid Logical. Whether to use the settings for the rapid assessment.
#'   * `TRUE`: uses `conus = TRUE`, `fill_empty_coords = TRUE` and
#'     `sf = TRUE`, whatever those arguments are set to. Also drops sources
#'     whose `source_type` mentions municipal, hauled, tribal or purchased
#'     water (any case), and sources with no `source_type`.
#'   * `FALSE`: uses `conus`, `fill_empty_coords` and `sf` as given, and keeps
#'     every source type.
#'
#'   Default: `FALSE`.
#' @param path String. Path to the WSD workbook (`.xlsx`). The function reads
#'   the first sheet, skips its first row, and treats `"NA"` and `"U"` as
#'   missing. If no file exists at `path`, it stops with an error. Default:
#'   `Water_Supply_Systems/NPS_Water_Systems_Database_Joined.xlsx` in the shared
#'   data folder, see [wava_data_path()].
#'
#' @return One row per water source, with the workbook's columns renamed to
#'   snake_case (`park_unit`, `wsd_source_id`, `source_type`, `state`, ...). An
#'   sf object of points if `sf = TRUE`, otherwise a tibble. With
#'   `fill_empty_coords = TRUE`, `coords_flag` says whether the coordinates come
#'   from the database (`"Source Coordinates"`) or were filled in
#'   (`"Centroid"`).
#' @export
#' @seealso [getParkSystem()], which supplies the park boundaries used to fill
#'   in coordinates; [wava_data_path()] for the default `path`.
#' @examplesIf file.exists(wava_data_path("Water_Supply_Systems", "NPS_Water_Systems_Database_Joined.xlsx"))
#' # Water sources for two parks, as points
#' wsd <- get_wsd(park = c("ROMO", "GRCA"))
#' head(wsd)
#'
#' # The whole database as a table, including sources with no coordinates
#' wsd_all <- get_wsd(conus = FALSE, sf = FALSE)
#' nrow(wsd_all)
#'
#' # The rapid assessment set (downloads park and state boundaries)
#' if (interactive()) {
#'   wsd_rapid <- get_wsd(rapid = TRUE)
#'   table(wsd_rapid$coords_flag)
#' }

get_wsd <- function(conus = TRUE,
                    fill_empty_coords = FALSE,
                    park = NULL,
                    sf = TRUE,
                    rapid = FALSE,
                    path = wava_data_path("Water_Supply_Systems",
                                          "NPS_Water_Systems_Database_Joined.xlsx")) {
  
  
  if (rapid == TRUE) {
    # override these args for rapid
    fill_empty_coords <- TRUE
    sf <- TRUE
    conus <- TRUE
  }
  
  # --- Error Checking: File Existence ---
  table_path <- path
  
  if (!file.exists(table_path)) {
    stop(paste("WSD file not found at:", table_path, 
               "\nPlease check your working directory or data path."))
  }
  
  # --- Load and Clean Water Supply Database ---
  WSD_raw <- readxl::read_excel(table_path, 
                                sheet = 1, 
                                na = c("NA", "U"), 
                                skip = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate(source_longitude = as.numeric(source_longitude),
                  source_latitude = as.numeric(source_latitude))
  
  if (rapid == TRUE) {
    WSD_raw <- WSD_raw %>%
      # Filter to CONUS & NPS-Operated Systems only
      dplyr::filter(!stringr::str_detect(source_type, 
                                         stringr::regex("municipal|hauled|tribal|purchased",
                                                        ignore_case = TRUE)))
  }
  
  # handle coordinate filling
  if (fill_empty_coords) {
    
    # Error checking for dependency
    if (!exists("getParkSystem")) {
      warning("Function 'getParkSystem()' not found. Skipping coordinate fill.")
    } else {
      parks <- getParkSystem()
      
      WSD_raw <- WSD_raw %>% 
        dplyr::mutate(coords_flag = ifelse(is.na(source_longitude) | is.na(source_latitude),
                                           "Centroid", "Source Coordinates"))
      
      # filter missing coords to only join those 
      WSD_missing <- WSD_raw %>% 
        dplyr::filter(coords_flag == "Centroid")
      
      # nothing missing, nothing to fill
      if (nrow(WSD_missing) == 0) {
        
        message("No missing coordinates found \u2014 skipping centroid fill step.")
        
      } else {
        
        # get associated park centroid and join...
        units_poly <- parks %>% 
          dplyr::filter(UNIT_CODE %in% WSD_missing$park_unit) %>% 
          sf::st_cast("POLYGON") %>% 
          sf::st_transform(5070)   # convert to meters projection
        
        # missing coords exist, but none matched a park polygon
        if (nrow(units_poly) == 0) {
          
          warning("No matching park units found for sites with missing coordinates \u2014 skipping centroid fill.")
          
        } else {
          
          # group by park and "split"
          exploded <- units_poly %>% 
            dplyr::group_split(UNIT_CODE)
          
          clustered <- exploded %>%
            purrr::map_dfr(function(p) {
              # If a park only has 1 polygon, don't waste time on it
              if(nrow(p) <= 1) {
                p$cluster_id <- 1
                return(p)
              }
              
              # use a simplified version of the geometry just for the distance check
              p_sim <- sf::st_simplify(p, dTolerance = 5)
              
              nb <- sf::st_is_within_distance(p_sim, dist = 30000)
              
              g <- igraph::graph_from_adj_list(nb)
              p$cluster_id <- igraph::components(g)$membership
              return(p)
            })
          
          # within each cluster (per park)
          units_clustered <- clustered %>% 
            dplyr::group_by(UNIT_CODE, cluster_id) %>% 
            dplyr::summarize(do_union = TRUE, .groups = "drop") %>% 
            sf::st_cast("MULTIPOLYGON") %>% 
            # st_transform(st_crs(getParkSystem())) %>%
            dplyr::mutate(cluster_id = as.character(cluster_id)) %>%
            sf::st_join(., tigris::states() %>% sf::st_transform(sf::st_crs(clustered)) %>% dplyr::select(STUSPS), left = TRUE) %>%
            sf::st_point_on_surface() %>%
            sf::st_transform(4326)
          
          WSD_missing_1 <- WSD_missing %>%
            dplyr::filter(!is.na(source_state)) %>%
            dplyr::left_join(units_clustered, by = c("park_unit" = "UNIT_CODE", "source_state" = "STUSPS")) 
          
          WSD_missing_2 <- WSD_missing %>%
            dplyr::filter(is.na(source_state)) %>%
            dplyr::left_join(units_clustered, by = c("park_unit" = "UNIT_CODE", "state" = "STUSPS")) 
          
          # Sites with so not enough information to discern which of the units it should be assigned to...
          # test <- bind_rows(WSD_missing_1, WSD_missing_2) %>%
          #   group_by(wsd_source_id) %>%
          #   mutate(count = n()) %>%
          #   filter(count > 1)
          
          WSD_missing_complete <- dplyr::bind_rows(WSD_missing_1, WSD_missing_2) %>%
            dplyr::distinct(wsd_source_id, .keep_all = TRUE) %>%
            dplyr::mutate(source_longitude = sf::st_coordinates(geometry)[, 1],
                          source_latitude = sf::st_coordinates(geometry)[, 2]) %>%
            sf::st_drop_geometry() %>%
            dplyr::select(-c(geometry, cluster_id))
          
          # join to rows that had coords
          WSD_raw <- WSD_raw %>%
            dplyr::filter(coords_flag == "Source Coordinates") %>% 
            dplyr::bind_rows(WSD_missing_complete)
          
        }
      }
      
    }
  }
  
  # --- Filters ---
  WSD <- WSD_raw
  
  if (!is.null(park)) {
    WSD <- WSD %>% dplyr::filter(park_unit %in% park)
  }
  
  if (conus) {
    # Territories/Oconus filters
    WSD <- WSD %>% 
      dplyr::filter(!state %in% c("AK", "AS", "HI", "MP", "PR", "VI", "GU")) 
  }
  
  # --- Return as Spatial or Flat ---
  if (sf) {
    # Final check: remove records that still have no coords after filling
    WSD <- WSD %>%
      tidyr::drop_na(source_longitude, source_latitude) %>%
      sf::st_as_sf(coords = c("source_longitude", "source_latitude"),
                   crs = 4326,
                   remove = FALSE)
  }
  
  return(WSD)
}