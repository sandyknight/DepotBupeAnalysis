#' Downdload, combine and de-duplicate the 23 and 24 UTLA data
#' @import data.table
#' @importFrom jsonlite read_json
#' @description Retrieves UTLA names and codes from Open Geography Portal ArcGIS
#' REST API
#' @return data.table containing UTLA codes and names for England
#' @details For some reason the data headers for 2023 and 2024 have different
#' naming conventions so that needs to be handled.
#' Also filters for English UTLAs (codes starting with 'E')
#' @export
get_geo_data <-
  function() {
    ## Get UTLAs 2023
    uri <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/UTLA_APR_2023_UK_NC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geo23json"

    geo23 <- jsonlite::read_json(uri, simplifyVector = TRUE)

    geo23 <- data.table::as.data.table(geo23[["features"]])

    data.table::setnames(geo23, tolower(gsub("properties\\.", "", names(geo23))))

    geo23 <- geo23[grep("^E", utla23cd, perl = TRUE), .(utla23cd, utla23nm)]

    data.table::setnames(geo23, function(x) gsub("utla23", "utla", x))

    ## Get UTLAs 2024

    uri <-
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/CTYUA_DEC_2024_UK_NC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

    geo24 <- jsonlite::read_json(uri, simplifyVector = TRUE)

    geo24 <- data.table::as.data.table(geo24[["features"]])

    data.table::setnames(geo24, tolower(gsub("properties\\.", "", names(geo24))))

    geo24 <- geo24[grep("^E", ctyua24cd, perl = TRUE), .(ctyua24cd, ctyua24nm)]

    data.table::setnames(geo24, function(x) gsub("ctyua24", "utla", x))

    # Combine unique code-name pairs
    geo <-
      unique(data.table::rbindlist(l = list(geo24, geo23)))

    return(geo)
  }
