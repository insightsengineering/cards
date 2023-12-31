% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functions.R
\name{summary_functions}
\alias{summary_functions}
\alias{categorical_variable_summary_fns}
\alias{continuous_variable_summary_fns}
\alias{missing_variable_summary_fns}
\title{Summary Functions}
\usage{
categorical_variable_summary_fns(
  summaries = c("n", "p", "N"),
  other_stats = NULL
)

continuous_variable_summary_fns(
  summaries = c("N", "mean", "sd", "median", "p25", "p75", "min", "max"),
  other_stats = NULL
)

missing_variable_summary_fns(
  summaries = c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss")
)
}
\arguments{
\item{summaries}{(\code{character})\cr
a character vector of results to include in output.
\itemize{
\item \code{categorical_variable_summary_fns()}: Select one or more from
'n', 'p', 'N'.
\item \code{continuous_variable_summary_fns()}: Select one or more from
'N', 'mean', 'sd', 'median', 'p25', 'p75', 'min', 'max'.
\item \code{missing_variable_summary_fns()}: Select one or more from
'N_obs', 'N_miss', 'N_nonmiss', 'p_miss', 'p_nonmiss'.
}}

\item{other_stats}{named list of other statistic functions to supplement the
pre-programmed functions.}
}
\value{
named list of summary functions
}
\description{
\itemize{
\item \code{continuous_variable_summary_fns()} returns a named list of summary functions
for continuous variables. Some functions include slight modifications to
their base equivalents. For example, the \code{min()} and \code{max()} functions
return \code{NA} instead of \code{Inf} when an empty vector is passed.
Statistics \code{"p25"} and \code{"p75"} are calculated with \code{quantile(type = 2)},
which matches
\href{https://psiaims.github.io/CAMIS/Comp/r-sas-summary-stats.html}{SAS's default value}.
\item \code{categorical_variable_summary_fns()} summary functions for categorical
variables. Options are \code{c("n", "N", "p")}. If a user requests, for example,
only \code{"p"}, the function will return \code{n} and \code{N} as well, since they are
needed to calculate \code{"p"}.
\item \code{missing_variable_summary_fns()} summary functions suitable for variable-level
summaries, such as number and rate of missing data.
}
}
\examples{
# continuous variable summaries
ard_continuous(
  ADSL,
  variables = "AGE",
  statistics = ~continuous_variable_summary_fns(c("N", "median"))
)

# categorical variable summaries
ard_categorical(
  ADSL,
  variables = "AGEGR1",
  statistics = ~categorical_variable_summary_fns(c("n", "N"))
)

# summary for rates of missing data
ard_missing(
  ADSL,
  variables = c("AGE", "AGEGR1"),
  statistics = ~missing_variable_summary_fns()
)
}
