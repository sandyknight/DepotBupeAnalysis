#' Validate merge operations
#' @import data.table
#' @description Checks for data integrity through merge operations
#' @param sir_df Original SIR data.table
#' @param main_df Original main data.table
#' @param final_df Merged data.table
#' @return Invisible NULL, throws assertion error if validation fails
#' @details
#' Checks that no records are unexpectedly gained
#' Verifies completeness of geographic mapping
#' @export
validate_merges <- function(sir_df, main_df, final_df) {
   
  # Check we haven't lost or gained any people through the merges
   
  initial_count <- main_df[, length(unique(client_random_id))]

  final_count <- final_df[, length(unique(client_random_id))]

  
  assertthat::assert_that(
    final_count <= initial_count,
    msg = "More ids after merge than before - check for duplicates"
  )
  
  # Check geography mapping is complete
  assertthat::assert_that(
    all(!is.na(final_df$utla23nm)),
    msg = "Missing UTLA names after geography merge"
  )
}
