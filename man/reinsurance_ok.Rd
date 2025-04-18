% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_checks.R
\name{reinsurance_ok}
\alias{reinsurance_ok}
\title{Checking reinsurance dataframe for possible mistakes}
\usage{
reinsurance_ok(reinsurance, indices, first_orig_year, last_orig_year)
}
\arguments{
\item{reinsurance}{Dataframe with reinsurance information, see details of \code{\link[=xl_cashflow]{xl_cashflow()}} or \link{reinsurance_xmpl}.}

\item{indices}{Dataframe for indexation, see details of \code{\link[=prepare_data]{prepare_data()}}.}

\item{first_orig_year}{First origin year.}

\item{last_orig_year}{Last origin year.}
}
\value{
TRUE if every check is passed, else FALSE.
}
\description{
This function checks this input data for possible mistakes, implausible entries or missing or double rows.
}
\examples{
reinsurance_ok(reinsurance_xmpl, indices_xmpl, 1989, 2023)
}
