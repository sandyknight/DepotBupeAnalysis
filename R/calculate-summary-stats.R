#' Calculate summary statistics including quartile ranges
#' @import data.table
#' @description Calculates comprehensive summary statistics for rates including quartile ranges
#' @param dt data.table containing analysis data
#' @param rate_col Character name of rate column (default: "lab_rate")
#' @param by_col Character name of grouping column (default: "year")
#' @return data.table with summary statistics including means, quartiles and ranges
#' @export
calculate_summary_stats <- function(dt, select_groups = c("year")) {
  
  # Quartiles 
  qtiles <- dt[, 
               lapply(.SD, quantile, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE),
               by = select_groups, 
               .SDcols = "lab_rate"
  ]
  
  qtiles[, Quartile := rep(
    c("Q1", "Q2", "Q3", "Max"), 
    times = .N / 4
  )]
  
  # Pivot wide 
  qtiles <- data.table::dcast(
    qtiles,
    formula = paste0(paste0(select_groups, collapse = " + "), " ~ Quartile"),
    value.var = "lab_rate"
  )
  
  dt2 <- dt[, .(
    phbudi_any = sum(phbudi_any, na.rm = TRUE),
    other_ost  = sum(other_ost,  na.rm = TRUE)
  ), by = select_groups]
  
  dt2[, lab_rate := phbudi_any / (phbudi_any + other_ost)]
  
  dt2[, all_ost := phbudi_any + other_ost]
  
  dtout <- data.table::merge.data.table(
    x = dt2,
    y = qtiles,
    by = select_groups,
    all.x = TRUE
  )
  
  # 4) Merge in the per-group IQR from the row-level distribution
  iqr_dt <- dt[, .(IQR = IQR(lab_rate, na.rm = TRUE)), by = select_groups]
  
  dtfinal <- data.table::merge.data.table(
    x = dtout,
    y = iqr_dt,
    by = select_groups,
    all.x = TRUE
  )
  
  dtfinal <- dtfinal[, !c("other_ost")]
  
  dtfinal <- dtfinal[, c(..select_groups, "phbudi_any", "all_ost", "lab_rate", "Q1", "Q2", "Q3", "Max", "IQR")]
  
  dtfinal <- data.table::setorderv(dtfinal, rev(select_groups))
    
  
  # Return final combined table
  return(dtfinal)
}
