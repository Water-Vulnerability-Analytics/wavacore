#' @title Get monthly visitation for NPS units
#'
#' @description
#' Gets monthly visitor counts for one or more NPS units from NPS Visitor Use
#' Statistics. By default it calls the IRMA STATS REST API, which returns
#' recreation and non-recreation visitors. With `allStats = TRUE` it downloads
#' the full visitation dataset from the NPS Data Store instead, which has every
#' statistic (visitor hours, overnight stays, campers and so on). Needs
#' internet.
#'
#' @param units Character vector of four-letter unit codes, e.g.
#'   `c("ROMO", "GRCA")`.
#' @param startMonth First month to include, 1-12. Default 1 (January).
#' @param startYear First year to include.
#' @param endMonth Last month to include, 1-12. Default 12 (December).
#' @param endYear Last year to include.
#' @param allStats Logical. If `TRUE`, returns every statistic from the full
#'   dataset (a larger, slower download). Default `FALSE`.
#'
#' @return A tibble with one row per unit and month. With `allStats = FALSE`,
#'   the columns are `UnitCode`, `UnitName`, `Year`, `Month`,
#'   `RecreationVisitors` and `NonRecreationVisitors`. With `allStats = TRUE`,
#'   they are `UnitCode`, `Year`, `Month` and one column per statistic (e.g.
#'   `RecreationVisitors`, `TotalVisitors`, `TotalOvernightStays`).
#' @export
#' @seealso [getParkSystem()] to look up unit codes (`UNIT_CODE`).
#' @examplesIf curl::has_internet()
#' # Monthly visitation for two parks in 2023
#' getUnitVisitation(c("ROMO", "GRCA"), startYear = 2023, endYear = 2023)
#'
#' # Summer months only
#' getUnitVisitation("ROMO", startMonth = 6, startYear = 2023,
#'                   endMonth = 8, endYear = 2023)
#'
#' # Every statistic, from the full dataset (slower)
#' if (interactive()) {
#'   getUnitVisitation("ROMO", startYear = 2023, endYear = 2023, allStats = TRUE)
#' }

getUnitVisitation <- function(units,
                              startMonth = 1,
                              startYear,
                              endMonth = 12,
                              endYear,
                              allStats = FALSE) {
  
  if (allStats == FALSE) {
    
    #base URL call
    call <- "https://irmaservices.nps.gov/stats/v1"
    
    units <- paste0(units, collapse = ",")
    
    dat <- httr::GET(paste0(call, "/visitation?unitCodes=", units, "&startMonth=",
                            startMonth, "&startYear=", startYear, "&endMonth=",
                            endMonth, "&endYear=", endYear))
    
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    #parse data in JSON
    dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
    
    #convert items to data.frame
    final_df <- tibble::as_tibble(dat_json)
    
  } else {
    
    # pull full visitation dataset from IRMA
    ref <- 2317666
    
    files <- jsonlite::fromJSON(paste0("https://irmaservices.nps.gov/datastore/v8/rest/Reference/", ref, "/DigitalFiles"),
                                flatten = TRUE)
    
    target <- files[files$fileName == "Main_Data.csv", ]
    
    dat <- readr::read_csv(curl::curl(target$downloadLink[1]),
                           show_col_types = FALSE)
    
    # filter to requested units and dates
    final_df <- dat %>%
      dplyr::filter(UnitCode %in% units,
                    Year > startYear | (Year == startYear & Month >= startMonth),
                    Year < endYear | (Year == endYear & Month <= endMonth)) %>%
      dplyr::mutate(Statistic = dplyr::recode(Statistic,
                                              "TRV"   = "RecreationVisitors",
                                              "TNRV"  = "NonRecreationVisitors",
                                              "TV"    = "TotalVisitors",
                                              "TRVH"  = "RecreationVisitorHours",
                                              "TNRVH" = "NonRecreationVisitorHours",
                                              "TH"    = "TotalVisitorHours",
                                              "CL"    = "ConcessionerLodging",
                                              "CCG"   = "ConcessionerCamping",
                                              "TT"    = "TentCampers",
                                              "TRVS"  = "RVCampers",
                                              "TTRV"  = "TotalNPSCampground",
                                              "BC"    = "BackcountryCampers",
                                              "MISC"  = "MiscellaneousOvernightStays",
                                              "NROS"  = "NonRecreationOvernightStays",
                                              "TOS"   = "TotalOvernightStays")) %>%
      tidyr::pivot_wider(names_from = Statistic,
                  values_from = Value)
    
  }
  
  return(final_df)
  
}