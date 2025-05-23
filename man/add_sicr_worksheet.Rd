% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Excel_Output.R
\name{add_sicr_worksheet}
\alias{add_sicr_worksheet}
\title{Add formatted worksheet to a workbook}
\usage{
add_sicr_worksheet(workbook, sheetname, payments, reserved = NULL)
}
\arguments{
\item{workbook}{Workbook created via \code{openxlsx2::wb_workbook()}.}

\item{sheetname}{Desired sheet name.}

\item{payments}{Numeric matrix of historic and future payments with origin years as rownames and origin years + 250
as calendar years as colnames.}

\item{reserved}{Default: NULL. Numeric matrix with historic payments. Not considered if NULL.}
}
\value{
Workbook including the new sheet.
}
\description{
This function adds a properly formatted worksheet with payments and reserves information to an existing workbook.
}
\examples{
future_payments <- matrix(0, 35, 250)
colnames(future_payments) <- 2024:2273
future_payments[,1] <- 1e6 # some entries to show functionality

payments <- cbind(all_claims_paid_xmpl, future_payments)

workbook <- openxlsx2::wb_workbook()
workbook <- add_sicr_worksheet(workbook = workbook,
                               sheetname = "new_sheet",
                               payments = payments,
                               reserved = all_claims_reserved_xmpl)

# either open with openxlsx2::wb_open(workbook) or save via openxlsx2::wb_save(workbook)
}
