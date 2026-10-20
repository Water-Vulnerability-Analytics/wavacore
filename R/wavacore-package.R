#' @keywords internal
"_PACKAGE"

## Functions from other packages that the code calls without a `pkg::` prefix.
#' @importFrom dplyr filter group_by mutate select summarize bind_rows distinct
#' @importFrom tidyr pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect regex
#' @importFrom purrr map_dfr
#' @importFrom sf st_cast st_transform st_join st_crs st_simplify
#' @importFrom sf st_point_on_surface st_coordinates st_drop_geometry
#' @importFrom utils download.file unzip
NULL

## Column names used inside dplyr verbs.
utils::globalVariables(c(
  ".", "Month", "PARKNAME", "REGION", "STATE", "STUSPS", "Statistic",
  "UNIT_CODE", "UNIT_NAME", "UnitCode", "Value", "Year", "cluster_id",
  "coords_flag", "geometry", "park_unit", "source_latitude",
  "source_longitude", "source_state", "source_type", "state",
  "wsd_source_id"
))
