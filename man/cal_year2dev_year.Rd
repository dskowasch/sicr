% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{cal_year2dev_year}
\alias{cal_year2dev_year}
\title{Transform calendar year triangle to development year triangle}
\usage{
cal_year2dev_year(cal_year_triangle)
}
\arguments{
\item{cal_year_triangle}{quadratic matrix with origin years as rows and calendar years as columns}
}
\value{
quadratic matrix with origin years as rows and development years as columns
}
\description{
Transform calendar year triangle to development year triangle
}
\examples{
cal_year_triangle <- matrix(c(1000, 0, 0, 200, 800, 0, 100, 100, 900), 3)
rownames(cal_year_triangle) <- colnames(cal_year_triangle) <- 2012:2014
print(cal_year_triangle)
cal_year2dev_year(cal_year_triangle)
}
