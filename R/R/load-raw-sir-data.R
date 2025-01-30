#' Load raw sub-intervention review (SIR) data
#' @import data.table
#' @description Loads and prepares the SIR dataset containing LAB treatment information
#' @return data.table containing all the SIR data
#' @details returns all columns
#' @export
load_raw_sir_data <- function(file_path = "inst/extdata/SIR_table_for_VfM_linked.csv") {
  
  sir_df <-
    data.table::fread(file_path)
  
  return(sir_df)
  
}