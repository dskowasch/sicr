% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{dev_year2cal_year}
\alias{dev_year2cal_year}
\title{Transform development year triangle to calendar year triangle}
\usage{
dev_year2cal_year(dev_year_triangle)
}
\arguments{
\item{dev_year_triangle}{quadratic matrix with origin years as rows and development years as columns}
}
\value{
quadratic matrix with origin years as rows and calendar years as columns
}
\description{
Transform development year triangle to calendar year triangle
}
\examples{
dev_year_triangle <- matrix(c(1000, 800, 900, 200, 100, 0, 100, 0, 0), 3)
rownames(dev_year_triangle) <- 2012:2014
colnames(dev_year_triangle) <- 1:3
print(dev_year_triangle)
dev_year2cal_year(dev_year_triangle)
}
