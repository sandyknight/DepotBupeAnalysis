#' Create formatted summary table
#' @import data.table
#' @description Creates a flextable table showing LAB rates by quartile with formatted percentages
#' @param summary_stats data.table containing output from calculate_summary_stats()
#' @return flextable object with formatted percentage ranges
#' @details
#' Formats all values as percentages with 1 decimal place
#' @export
create_summary_table <- function(summary_stats) {
  data.table::setnames(summary_stats, old = "phbudi_any", new = "LAB_count", skip_absent = TRUE)

  nms <- names(summary_stats)

  nms <- gsub("_", " ", nms)

  sentence_case_helper <-
    function(i) {
      paste0(toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)))
    }

  nms <- data.table::fcase(grepl("lab", nms),
    gsub("lab", "LAB", nms),
    grepl("ost", nms),
    gsub("ost", "OST", nms),
    grepl("q", nms),
    gsub("q", "Q", nms),
    default = nms
  )
  nms <- sapply(nms, sentence_case_helper)

  data.table::setnames(summary_stats, nms)

  groups <- grep("Tranche|Year|count|All",
    names(summary_stats),
    perl = TRUE,
    value = TRUE,
    ignore.case = TRUE
  )

  summary_stats <-
    summary_stats[, lapply(.SD, function(x) sprintf("    %.2f%%", x * 100)), by = groups, .SDcols = c("LAB rate", "Q1", "Q2", "Q3", "Max", "IQR")]

  ft_table <-
    flextable::flextable(summary_stats) |>
    flextable::colformat_num(j = "Year", digits = 0, big.mark = "") |>
    flextable::colformat_num(j = c("LAB count", "All OST"), digits = 0, big.mark = ",") |>
    flextable::theme_box() |>
    flextable::autofit() |>
    flextable::align(align = "left", part = "all")

  return(ft_table)
}
