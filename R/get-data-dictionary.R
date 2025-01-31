#' Get data dictionary
#' @import data.table
#' @description Loads and combines data dictionaries from RAND Excel file
#' @param dictionary_xlsx Path to Excel file containing data dictionary
#' @return data.table containing combined data dictionary
#' @details
#' Checks for existing CSV version first
#' Combines dictionaries from main, SIR, and TOP sheets
#' Cleans column names
#' @export
get_data_dictionary <-
  function(dictionary_xlsx = "data/RAND-data-dictionary.xlsx") {
    if (file.exists("data/data-dictionary.csv")) {
      return(data.table::fread("data/data-dictionary.csv"))
    }

    dd_main <-
      openxlsx::read.xlsx(
        xlsxFile = "data/RAND-data-dictionary.xlsx",
        sheet = "Main table - journeys",
        cols = c(1, 2, 4:6)
      ) |>
      janitor::clean_names() |>
      dplyr::rename("column" = column_name)

    dd_sir <-
      openxlsx::read.xlsx(
        xlsxFile = "data/RAND-data-dictionary.xlsx",
        sheet = "SIR table",
        cols = c(1:5)
      ) |>
      janitor::clean_names()

    dd_top <-
      openxlsx::read.xlsx(
        xlsxFile = "data/RAND-data-dictionary.xlsx",
        sheet = "TOP table",
        cols = c(1:5)
      ) |>
      janitor::clean_names()

    data_dictionary <- data.table::rbindlist(l = list(dd_main, dd_sir, dd_top))
    data.table::fwrite(data_dictionary, "data/data-dictionary.csv")
    data_dictionary
  }
