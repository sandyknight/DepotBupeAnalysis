#' Load main treatment data
#' @import data.table
#' @description Loads and filters the main treatment dataset for opiate users
#' @return data.table containing filtered client IDs, journey numbers and UTLA codes
#' @details Filters for drug_grp == "Opiate" and selects only needed columns
#' @export
load_main_data  <- function(file_path = "inst/extdata/K3anon_FullDataset_for_VfM.csv") { 
  main_df <-
    data.table::fread(file_path)

  main_df <-
    main_df[drug_grp == "Opiate", .(client_random_id, n_jy, utla23cd)]
  
  return(main_df)
}


