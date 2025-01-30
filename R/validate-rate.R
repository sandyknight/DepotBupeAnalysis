#' Validate rate calculations
#' @import data.table
#' @description Checks validity of calculated rates
#' @param df data.table containing rate calculations
#' @return Invisible NULL, throws assertion error if validation fails
#' @details
#' Verifies rates are between 0 and 1
#' Cross-checks rate calculations against raw counts
#' @export
validate_rate <- function(df) {
  # Check rates are between 0 and 1
  assertthat::assert_that(
    all(df$lab_rate >= 0 & df$lab_rate <= 1),
    msg = "Invalid rates detected"
  )
  
  # Verify rate calculation matches raw counts
  df[, {
    calculated_rate <- phbudi_any / (phbudi_any + other_ost)
    assertthat::assert_that(
      all(abs(calculated_rate - lab_rate) < 1e-10),
      msg = "Rate calculations don't match raw counts"
    )
  }]
}
