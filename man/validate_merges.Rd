% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate-merges.R
\name{validate_merges}
\alias{validate_merges}
\title{Validate merge operations}
\usage{
validate_merges(sir_df, main_df, final_df)
}
\arguments{
\item{sir_df}{Original SIR data.table}

\item{main_df}{Original main data.table}

\item{final_df}{Merged data.table}
}
\value{
Invisible NULL, throws assertion error if validation fails
}
\description{
Checks for data integrity through merge operations
}
\details{
Checks that no records are unexpectedly gained
Verifies completeness of geographic mapping
}
