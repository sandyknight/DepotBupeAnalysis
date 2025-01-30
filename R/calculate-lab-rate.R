#' Calculate LAB treatment rates
#' @import data.table
#' @description Calculates counts and rates of LAB treatment by specified grouping variables
#' @param dt data.table containing treatment data
#' @param groups Character vector of column names to group by (default: "year")
#' @return data.table with LAB counts and rates by group
#' @details Calculates lab_rate as phbudi_any/(phbudi_any + other_ost)
#' @export
calculate_lab_rate <-
  function(dt, groups = "year") {

    dt <-
      dt[, .(
        phbudi_any = sum(phbudi_any),
        other_ost = sum(other_ost)
      ), by = groups]

    dt[, lab_rate := phbudi_any / (phbudi_any + other_ost)]

    return(dt)

  }
