% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_statistic_fmt_fn.R
\name{apply_statistic_fmt_fn}
\alias{apply_statistic_fmt_fn}
\title{Apply Formatting Functions}
\usage{
apply_statistic_fmt_fn(x)
}
\arguments{
\item{x}{(\code{data.frame})\cr
an ARD data frame of class 'card'}
}
\value{
an ARD data frame
}
\description{
Apply the formatting functions to each of the raw statistics.
}
\examples{
ard_continuous(ADSL, variables = "AGE") |>
  apply_statistic_fmt_fn()
}
