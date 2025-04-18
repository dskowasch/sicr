% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_checks.R
\name{claims_data_ok}
\alias{claims_data_ok}
\title{Checking claims_data dataframe for possible mistakes}
\usage{
claims_data_ok(claims_data, last_orig_year)
}
\arguments{
\item{claims_data}{Dataframe, see details of \code{\link[=prepare_data]{prepare_data()}}.}

\item{last_orig_year}{Last origin year.}
}
\value{
TRUE if every check is passed, else FALSE.
}
\description{
This function checks this input data for possible mistakes, implausible entries or missing or double rows.
}
\examples{
claims_data_ok(claims_data_xmpl, 2023)
}
