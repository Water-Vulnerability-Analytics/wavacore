#' Root folder of the shared WaVA data
#'
#' Looks for the folder in this order:
#' 1. `options(wava.data_dir = ...)`
#' 2. the `WAVA_DATA_DIR` environment variable (an empty value counts as unset)
#' 3. `"data"` in the working directory (the link to the shared OneDrive folder
#'    that every WaVA repo already has)
#'
#' @return A path as a character string.
#' @export
wava_data_dir <- function() {
  env <- Sys.getenv("WAVA_DATA_DIR")
  getOption("wava.data_dir", if (nzchar(env)) env else "data")
}

#' Build a path inside the shared WaVA data folder
#'
#' @param ... Path pieces below the data root, as in [file.path()].
#' @return A path as a character string.
#' @export
#' @examples
#' wava_data_path("rapid", "inputs")
wava_data_path <- function(...) file.path(wava_data_dir(), ...)
