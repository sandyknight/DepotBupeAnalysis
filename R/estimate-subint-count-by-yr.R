#' Estimate annual count of clients receiving a subintervention
#' @import data.table
#' @description Returns a data table with an estimated count for subintervention
#' by year
#' @param dt `data.table` with columns client_random, n_jy, submoddt +
#' a subintervention binary variable and groups required.
#' @param subintervention Character the quoted name of the subintervention
#' column (default: "phbudi_any")
#' @param groupby Character the quoted names of desired grouping variables
#' (default: NULL)
#' @returns a data table with 4 (+ length of `groupby`) columns
#' @details We cannot count by the `submoddt` month, since we would be
#' measuring the number of SIRs per month, not the number of clients.
#' This count is an *estimate* since it relies on a choice to count a
#' client with one (or more) SIR indicating the subintervention as
#' being counted as receiving that subintervention. It is possible,
#' although unlikely, that a client could be counted after only
#' receiving the subintervention for a few days.
#' @export
estimate_subint_count_by_yr <-
  function(dt, subintervention = "phbudi_any", groupby = NULL) {

    # Remove post-discharge SIRs since we want people in treatment
    dt <- dt[post_discharge == "N", ]

    # Required columns
    base_cols <- c("year", "client_random_id", "n_jy", "submoddt", subintervention)

    # Additional grouping columns
    all_cols <- if (!is.null(groupby)) c(base_cols, groupby) else base_cols

    # Select and rename the columns
    dt <- dt[, ..all_cols]

    # Add an `other_ost` column: 1 if `subintervention` is NOT 1, otherwise 0
    dt[, other_ost := data.table::fifelse(get(subintervention) == 1L, 0L, 1L)]

    # 2. Aggregate by year AND unique client ID (plus `groupby` if provided)
    group_vars <- c("client_random_id", "year", groupby)

    dt <-
      dt[, lapply(.SD, sum),
         by = group_vars,
         .SDcols = c(subintervention, "other_ost")]

    # 3. Convert to back to binary: if SIR indicated subintervention
    # once or more within a year then 1, else 0
    dt <-
      dt[, lapply(.SD, function(x) data.table::fifelse(x > 0, 1, 0)),
         by = c("year", groupby),
         .SDcols = c(subintervention, "other_ost")]

    # 4. Add total OST column
    dt[, total_ost := get(subintervention) + other_ost]

    # 5. Sum by year (and `groupby` if provided)
    dt <-
      dt[, lapply(.SD, sum), by = c("year", groupby)]

    return(dt)
  }
