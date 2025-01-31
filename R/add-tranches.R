#' Add SSMTR tranches to a data table
#' @import data.table
#' @description Loads tranche information, add it to dt and cleans up
#' inconsistencies
#' @param dt data.table with a column containing UTLA codes
#' @param utlacd_col Character name of column containing UTLA codes
#' @param tranches_file_path Character file path to tranche assigments data
#' @return data.table with added tranche column
#' @details Each upper-tier local authority was assigned a "tranche" number in
#' in 2022 which determined the amount and timing of additional funding they'd
#' receive for drug and alcohol treatment.
#'
#' This function adds the tranche number to any data table with a utla23cd
#' column
#' @export
add_tranches <-
  function(dt, utlacd_col = "utla23cd", utlanm_col = "utla23nm", tranches_file_path) {

    trch <- data.table::fread(tranches_files_path)

    trch <- trch[, .(`Area code`, Tranche)]

    data.table::setnames(trch, c(utlacd_col, "tranche"))

    dt <- data.table::merge.data.table(df, trch, by = utlacd_col, all.x = TRUE)

    dt[, tranche := as.integer(tranche)]

    ## dt[, tranche := data.table::fcase(utlanm == "West Northamptonshire",
    ##                                   2L,
    ##                                   utlanm == "North Yorkshire",
    ##                                   3L,
    ##                                   utlanm == "Somerset",
    ##                                   3L,
    ##                                   default = tranche)]

    return(dt)
  }
