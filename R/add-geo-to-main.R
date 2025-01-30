#' Add geographic names to main dataset
#' @import data.table
#' @description Merges UTLA names with main dataset using UTLA codes
#' @param main data.table containing main dataset with utla23cd column
#' @param geo data.table containing UTLA reference data
#' @return data.table with added utla23nm column
#' @export
add_geo_to_main_df <-
  function(main, geo) {

    df <- data.table::merge.data.table(main, geo, by.x = "utla23cd", by.y = "utlacd")

    trch <- data.table::fread("inst/extdata/published_allocations_tranches.csv")

    trch <- trch[, .(`Area code`, Tranche)]

    data.table::setnames(trch, c("utla23cd", "tranche"))

    dt <- data.table::merge.data.table(df, trch, by = "utla23cd", all.x = TRUE)

    dt[, tranche := as.integer(tranche)]

    dt[, tranche := data.table::fcase(utlanm == "West Northamptonshire",
                                      2L,
                                      utlanm == "North Yorkshire",
                                      3L,
                                      utlanm == "Somerset",
                                      3L,
                                      default = tranche)]

    data.table::setnames(dt, old = "utlanm", new = "utla23nm")


    return(dt)
  }
