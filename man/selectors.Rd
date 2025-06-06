% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectors.R
\name{selectors}
\alias{selectors}
\alias{all_ard_groups}
\alias{all_ard_variables}
\alias{all_ard_group_n}
\alias{all_missing_columns}
\title{ARD Selectors}
\usage{
all_ard_groups(types = c("names", "levels"))

all_ard_variables(types = c("names", "levels"))

all_ard_group_n(n, types = c("names", "levels"))

all_missing_columns()
}
\arguments{
\item{types}{(\code{character})\cr
type(s) of columns to select. \code{"names"} selects the columns variable name columns,
and \code{"levels"} selects the level columns. Default is \code{c("names", "levels")}.}

\item{n}{(\code{integer})\cr
integer(s) indicating which grouping columns to select.}
}
\value{
tidyselect output
}
\description{
These selection helpers match variables according to a given pattern.
\itemize{
\item \code{all_ard_groups()}: Function selects grouping columns, e.g. columns
named \code{"group##"} or \code{"group##_level"}.
\item \code{all_ard_variables()}: Function selects variables columns, e.g. columns
named \code{"variable"} or \code{"variable_level"}.
\item \code{all_ard_group_n()}: Function selects \code{n} grouping columns.
\item \code{all_missing_columns()}: Function selects columns that are all \code{NA} or empty.
}
}
\examples{
ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

ard |> dplyr::select(all_ard_groups())
ard |> dplyr::select(all_ard_variables())
}
