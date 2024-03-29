% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.is_named_list}
\alias{.is_named_list}
\title{Named List Predicate}
\usage{
.is_named_list(x, allow_df = FALSE)
}
\arguments{
\item{x}{(\code{any})\cr
object to check}
}
\value{
a logical
}
\description{
A predicate function to check whether input is a named list and \emph{not} a data frame.
}
\examples{
cards:::.is_named_list(list(a = 1:3))
}
\keyword{internal}
