#' @keywords internal
"_PACKAGE"

## Functions from other packages are called as `pkg::fun()`. The only import
## is `%>%` (operators can't be called with `::`), see utils-pipe.R.

## Column names used inside dplyr verbs.
utils::globalVariables(c(
  ".", "Month", "PARKNAME", "REGION", "STATE", "STUSPS", "Statistic",
  "UNIT_CODE", "UNIT_NAME", "UnitCode", "Value", "Year", "cluster_id",
  "coords_flag", "geometry", "park_unit", "source_latitude",
  "source_longitude", "source_state", "source_type", "state",
  "wsd_source_id"
))
