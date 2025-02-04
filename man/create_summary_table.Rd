% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-summary-table.R
\name{create_summary_table}
\alias{create_summary_table}
\title{Create formatted summary table}
\usage{
create_summary_table(summary_stats)
}
\arguments{
\item{summary_stats}{data.table containing output from calculate_summary_stats()}
}
\value{
flextable object with formatted percentage ranges
}
\description{
Creates a flextable table showing LAB rates by quartile with formatted percentages
}
\details{
Formats all values as percentages with 1 decimal place
}
