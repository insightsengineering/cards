% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.purrr_list_flatten}
\alias{.purrr_list_flatten}
\title{A list_flatten()-like Function}
\usage{
.purrr_list_flatten(x)
}
\arguments{
\item{x}{(named \code{list})\cr
a named list}
}
\value{
a named list
}
\description{
Function operates similarly to \code{purrr::list_flatten(x, name_spec = "{inner}")}.
}
\examples{
x <- list(a = 1, b = list(b1 = 2, b2 = 3), c = list(c1 = 4, c2 = list(c2a = 5)))

cards:::.purrr_list_flatten(x)
}
\keyword{internal}
