#' Load sub-intervention review (SIR) data
#' @import data.table
#' @description Loads and prepares the SIR dataset containing LAB treatment information
#' @return data.table containing client IDs, journey numbers, review dates and LAB status
#' @details Selects only the columns needed for LAB analysis
#' @export
load_sir_data  <- function(file_path = "inst/extdata/SIR_table_for_VfM_linked.csv") { 

  sir_df <-
    data.table::fread(file_path)
  
  return(sir_df)

  sir_df <-
    sir_df[, .(client_random_id, n_jy, submoddt, phbudi_any, phbupren_any, date_order)]

  sir_df <-sir_df[lubridate::year(submoddt) > 2019, ]
  return(sir_df)

}

