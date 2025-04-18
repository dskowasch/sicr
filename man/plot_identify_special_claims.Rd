% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_identify_special_claims}
\alias{plot_identify_special_claims}
\title{Generates plot to identify special claims}
\usage{
plot_identify_special_claims(
  pools,
  extended_claims_data,
  first_orig_year,
  last_orig_year,
  selected_reserve_classes,
  special_claims = NULL
)
}
\arguments{
\item{pools}{List as output of \code{\link[=generate_pools]{generate_pools()}}.}

\item{extended_claims_data}{Prepared claims data dataframe, see details of \code{\link[=prepare_data]{prepare_data()}}.}

\item{first_orig_year}{First desired origin year.}

\item{last_orig_year}{Last desired origin year.}

\item{selected_reserve_classes}{Reserve classes to be shown in the plot.}

\item{special_claims}{Dataframe with special claims with at least the column \code{Claim_id}. May be Null if
no special claim is identified. Default: NULL}
}
\value{
ggplot2 object
}
\description{
This function generates a plot that graphically compares the entry reserves of the historic observations
with the entry reserves of the claims that shall be simulated. It helps to identify claims without sufficient
similar historic claims for the simultaion.
}
\examples{
# this example uses data provided with this package
extended_claims_data <- prepare_data(claims_data = claims_data_xmpl,
                                     indices = indices_xmpl,
                                     threshold = 400000,
                                     first_orig_year = 1989,
                                     last_orig_year = 2023,
                                     expected_year_of_growing_large = 3,
                                     reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                                     pool_of_annuities = pool_of_annuities_xmpl)

pools <- generate_pools(extended_claims_data = extended_claims_data,
                        reserve_classes = c(1, 200001, 400001, 700001, 1400001),
                        years_for_pools = 2014:2023,
                        start_of_tail = 17,
                        end_of_tail = 50,
                        lower_outlier_limit = -Inf,
                        upper_outlier_limit = Inf,
                        pool_of_annuities = pool_of_annuities_xmpl)
p <- plot_identify_special_claims(
         pools = pools,
         extended_claims_data = extended_claims_data,
         first_orig_year = 1989,
         last_orig_year = 2023,
         selected_reserve_classes = 4:5,
         special_claims = NULL)
}
