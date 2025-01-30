#' Load raw treatment data
#' @import data.table
#' @description Loads and filters the main treatment dataset for opiate users
#' @return data.table filtered only to Opiate clients
#' @details Filters for drug_grp == "Opiate" and retains all columns
#' @export
load_raw_main_data  <- function(file_path = "inst/extdata/K3anon_FullDataset_for_VfM.csv") { 
  main_df <-
    data.table::fread(file_path)
  
  main_df <-
    main_df[drug_grp == "Opiate", ]
  
  return(main_df)
}
