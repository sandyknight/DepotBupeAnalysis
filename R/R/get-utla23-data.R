#' Get UTLA 2023 geographic reference data
#' @import data.table
#' @importFrom jsonlite read_json
#' @description Retrieves UTLA names and codes from official ArcGIS REST API
#' @return data.table containing UTLA codes and names for England
#' @details Filters for English UTLAs (codes starting with 'E')
#' @export
get_utla23_data <- function() {

  uri <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/UTLA_APR_2023_UK_NC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

  geo <- jsonlite::read_json(uri, simplifyVector = TRUE)

  geo <- data.table::as.data.table(geo[["features"]])

  data.table::setnames(geo, tolower(gsub("properties\\.", "", names(geo))))

  geo <- geo[grep("^E", utla23cd, perl = TRUE), .(utla23cd, utla23nm)]

  data.table::setnames(geo, function(x) gsub("utla23", "utla", x))

  return(geo)

}
