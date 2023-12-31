% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.unique_and_sorted}
\alias{.unique_and_sorted}
\title{ARD-flavor of unique()}
\usage{
.unique_and_sorted(x)
}
\arguments{
\item{x}{a vector}
}
\value{
a vector
}
\description{
Essentially a wrapper for \code{unique(x) |> sort()} with NA levels removed.
Expect in the case of factor and logical vectors.
For factors, all levels are returned even if they are unobserved.
Similarly, logical vectors always return \code{c(TRUE, FALSE)}, even if
both levels are not observed.
}
\keyword{internal}
