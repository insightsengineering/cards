% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shift_ard_columns.R
\name{.pair_columns}
\alias{.pair_columns}
\title{Pair columns}
\usage{
.pair_columns(x, columns)
}
\arguments{
\item{x}{(\code{data.frame})\cr
a data frame}

\item{columns}{(\code{character})\cr
all columns to consider for coalescing/renaming}
}
\value{
a list of column pairs (as character vectors)
}
\description{
This function ingests an ARD object and finds pairs of columns based on those requested for coalescing/renaming
}
\examples{
ard_categorical(ADSL, by = "ARM", variables = "AGEGR1") |>
  cards:::.pair_columns(columns = c("group1", "group1_level", "variable", "variable_level"))
}
\keyword{internal}
