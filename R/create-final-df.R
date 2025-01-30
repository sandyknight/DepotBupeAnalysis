#' Create final analysis dataset
#' @import data.table
#' @description Prepares and merges main and SIR data for analysis
#' @param sir_df data.table containing SIR data
#' @param main_df data.table containing main treatment data
#' @return data.table containing merged and prepared data for analysis
#' @details
#' 1. Adds geographic names to main data
#' 2. Creates other_ost indicator
#' 3. Merges SIR and main data
#' @export
create_final_df <-
  function(sir_df, main_df) {

    final_df <-
      data.table::merge.data.table(sir_df, main_df, by = c("client_random_id", "n_jy"))


    return(final_df)

}
