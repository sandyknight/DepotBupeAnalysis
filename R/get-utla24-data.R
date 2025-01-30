#' Get UTLA 2024 geographic reference data
#' @import data.table
#' @description Retrieves UTLA names and codes from official ArcGIS REST API
#' @return data.table containing UTLA codes and names for England
#' @details Filters for English UTLAs (codes starting with 'E')
#' @export
get_utla24_data <- function() {
  uri <-
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CTYUA_DEC_2024_UK_NC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

  geo <- jsonlite::read_json(uri, simplifyVector = TRUE)

  geo <- data.table::as.data.table(geo[["features"]])

  data.table::setnames(geo, tolower(gsub("properties\\.", "", names(geo))))

  geo <- geo[grep("^E", ctyua24cd, perl = TRUE), .(ctyua24cd, ctyua24nm)]

  data.table::setnames(geo, function(x) gsub("ctyua24", "utla", x))

  return(geo)
}
