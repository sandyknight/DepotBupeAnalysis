#' Render formatted data dictionary
#' @import data.table
#' @description Creates a formatted GT table of the data dictionary
#' @return gt_table object containing formatted data dictionary
#' @details
#' Filters dictionary to only show relevant columns
#' Formats column names in sentence case
#' @export
render_data_dictionary <-
  function() {
    dd <- get_data_dictionary()

    rgx <-
      paste(c(colnames(sir_df),
              colnames(main_df)),
            collapse = "|")

    dd <-
      dd[grep(pattern = rgx, x = column, perl = TRUE), ]

    data.table::setnames(dd, new = snakecase::to_sentence_case)

    gt::gt(dd)
}
