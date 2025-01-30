#' Unzip inst/extdata files and check contents
#' @description Unzips the RAND LAB inst/extdataset if not already unzipped and returns list of available files
#' @param zipfile Character string specifying the name of the zip file (default: "RAND-LAB-inst/extdataset.zip")
#' @return Character vector of CSV filenames in the inst/extdata directory
#' @export
unzip_datafile <- function(zipfile = "RAND-LAB-dataset.zip")
  if (!file.exists("inst/extdata/K3anon_FullDataset_for_VfM.csv")) {
    unzip(file.path("inst", "extdata", zipfile), exdir = "inst/extdata")
    list.files("inst/extdata/", pattern = "\\.csv")
  } else {
    list.files("inst/extdata/", pattern = "\\.csv")
  }


