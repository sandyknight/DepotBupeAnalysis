#' Combine and de-duplicate the 23 and 24 UTLA data
#' @import data.table
#' @description Retrieves UTLA names and codes from official ArcGIS REST API
#' @return data.table containing UTLA codes and names for England
#' @details Filters for English UTLAs (codes starting with 'E')
#' @export
get_geo_data <-
  function() {
    utla24_dt <-
      get_utla24_data()

    utla23_dt <-
      get_utla23_data()

    geo <-
      unique(data.table::rbindlist(l = list(utla24_dt, utla23_dt)))

    return(geo)
}
