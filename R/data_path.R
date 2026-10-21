#' @title Root folder of the shared WaVA data
#'
#' @description
#' Returns the folder that holds the shared WaVA data (the `data` folder in the
#' OneDrive shared library). Functions in `wavacore`, `wavarapid`,
#' `wavadeepdive` and `npswbm` build their default file paths from it, through
#' [wava_data_path()].
#'
#' Looks for the folder in this order:
#' 1. `options(wava.data_dir = ...)`
#' 2. the `WAVA_DATA_DIR` environment variable (an empty value counts as unset)
#' 3. `"data"` in the working directory (the link to the shared OneDrive folder
#'    that every WaVA repo already has)
#'
#' @details
#' Which one to use:
#' * **Environment variable** (recommended; set it once per computer). Open
#'   your user `.Renviron` file with `usethis::edit_r_environ()`, add a line
#'   like the one below, and restart R. Keep the quotes: the OneDrive path has
#'   spaces.
#'
#'   ```
#'   WAVA_DATA_DIR="~/Library/CloudStorage/OneDrive-SharedLibraries-Colostate/WCNR NPS-CSU-DRI - Water for People (WFP)/data"
#'   ```
#' * **Option** (one session or one script). `options(wava.data_dir = "...")`
#'   at the top of a script overrides the environment variable until R
#'   restarts.
#' * **`data` folder** (nothing to set). Works when the working directory has a
#'   `data` folder or link, as in `nps_water_vulnerability`. When you knit an
#'   Rmd that sits in a subfolder, the working directory is that subfolder, so
#'   use one of the other two.
#'
#' @return The path to the data folder, as a character string. The function
#'   does not check that the folder exists.
#' @export
#' @seealso [wava_data_path()] to build a path inside the data folder.
#' @examples
#' # Where the data folder is in this session
#' wava_data_dir()
#'
#' # Save the current settings so the examples below can put them back
#' old_opt <- options(wava.data_dir = NULL)
#' old_env <- Sys.getenv("WAVA_DATA_DIR", unset = NA)
#' Sys.unsetenv("WAVA_DATA_DIR")
#'
#' # 3. Nothing set: "data" in the working directory
#' wava_data_dir()
#'
#' # 2. The environment variable. Normally you set it once in ~/.Renviron;
#' # Sys.setenv() sets it for this session only.
#' Sys.setenv(WAVA_DATA_DIR = "/path/to/shared/data")
#' wava_data_dir()
#'
#' # 1. The option, which beats the environment variable
#' options(wava.data_dir = "/another/data/folder")
#' wava_data_dir()
#'
#' # Put the settings back
#' options(old_opt)
#' if (is.na(old_env)) Sys.unsetenv("WAVA_DATA_DIR") else Sys.setenv(WAVA_DATA_DIR = old_env)

wava_data_dir <- function() {
  env <- Sys.getenv("WAVA_DATA_DIR")
  getOption("wava.data_dir", if (nzchar(env)) env else "data")
}

#' @title Build a path inside the shared WaVA data folder
#'
#' @description
#' Joins path pieces onto the data folder from [wava_data_dir()], the same way
#' as [file.path()]. Package functions use it for their default file paths, so
#' they find the shared data wherever it is on your computer.
#'
#' @param ... Strings. Path pieces below the data folder, as in [file.path()].
#'   * One or more pieces, e.g. `"rapid", "inputs"`: returns
#'     `<data folder>/rapid/inputs`.
#'   * No pieces: returns the data folder itself, the same as
#'     [wava_data_dir()].
#' @return A path as a character string. The function does not check that the
#'   file exists.
#' @export
#' @seealso [wava_data_dir()] for how the data folder is found.
#' @examples
#' wava_data_path("rapid", "inputs")
#'
#' # The water supply database that get_wsd() reads by default
#' wava_data_path("Water_Supply_Systems", "NPS_Water_Systems_Database_Joined.xlsx")
#'
#' # Check whether a file is there before reading it
#' file.exists(wava_data_path("Water_Supply_Systems", "NPS_Water_Systems_Database_Joined.xlsx"))

wava_data_path <- function(...) file.path(wava_data_dir(), ...)
