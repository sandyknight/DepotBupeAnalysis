#' Validate data aggregation
#' @import data.table
#' @importFrom assertthat assert_that
#' @description Ensures totals are correct through aggregation steps
#' @param final_df Pre-aggregation data.table
#' @param aggregated_df Post-aggregation data.table
#' @return Invisible NULL, throws assertion error if validation fails
#' @details This validates the method for taking an annual count of LAB
#' @export
validate_aggregation <- function(final_df, aggregated_df) {
# Sum the total phbudi_any in the pre-aggregation dt by year AND unique client ID 

dt0a <- aggregated_df[, .(lab_total = sum(phbudi_any)), by = .(year, client_random_id)]

# Filter to unique clients with more than one phbudi_any = 1 within a year
# i.e. clients who had multiple SIRs within the calendar year each of which produced 
# a row of phbudi_any == 1.

count_of_multi_sir <- 
  dt0a[lab_total > 1, .(year, client_random_id, lab_total =  lab_total - 1)][, sum(lab_total)]

# Show that the difference before and after aggregation is equal to the count of
# multiple SIRs - 1 (the one we retained as a count for that year)

assertthat::assert_that((dt0[, sum(phbudi_any)] - final_df[, sum(phbudi_any)]) == count_of_multi_sir)

}
