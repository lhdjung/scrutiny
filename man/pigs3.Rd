% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-doc.R
\docType{data}
\name{pigs3}
\alias{pigs3}
\title{Binary means and standard deviations for using DEBIT}
\format{
A tibble (data frame) with 7 rows and 3 columns. The columns are:
\describe{
\item{x}{String. Means.}
\item{sd}{String. Standard deviations.}
\item{n}{Numeric. Sample sizes.}
}
}
\usage{
pigs3
}
\value{
A tibble (data frame).
}
\description{
A fictional dataset with means and standard deviations from a binary
distribution related to flying pigs. It can be used to demonstrate the
functionality of \code{debit_map()} and functions building up on it.
}
\seealso{
\code{pigs1} for GRIM-testing means, \code{\link{pigs2}} for GRIM-testing
percentages, and \code{\link{pigs4}} for detecting duplicates.

\code{pigs1} for GRIM-testing means, \code{\link{pigs2}} for GRIM-testing
percentages instead of means, \code{\link{pigs4}} for detecting duplicates, and
\code{\link{pigs5}} for GRIMMER-testing.
}
\keyword{datasets}
