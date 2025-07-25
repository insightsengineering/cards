% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_ard.R
\name{bind_ard}
\alias{bind_ard}
\title{Bind ARDs}
\usage{
bind_ard(
  ...,
  .distinct = TRUE,
  .update = FALSE,
  .order = FALSE,
  .quiet = FALSE
)
}
\arguments{
\item{...}{(\code{\link[rlang:dyn-dots]{dynamic-dots}})\cr
ARDs to combine. Each argument can either be an ARD,
or a list of ARDs. Columns are matched by name, and any missing
columns will be filled with \code{NA}.}

\item{.distinct}{(\code{logical})\cr
logical indicating whether to remove non-distinct values from the ARD.
Duplicates are checked across grouping variables, primary variables,
context (if present), the \strong{statistic name and the statistic value}.
Default is \code{TRUE}. If a statistic name and value is repeated and \code{.distinct=TRUE},
the more recently added statistics will be retained, and the other(s) omitted.}

\item{.update}{(\code{logical})\cr
logical indicating whether to update ARD and remove duplicated named statistics.
Duplicates are checked across grouping variables, primary variables, and the
\strong{statistic name}.
Default is \code{FALSE}. If a statistic name is repeated and \code{.update=TRUE},
the more recently added statistics will be retained, and the other(s) omitted.}

\item{.order}{(\code{logical})\cr
logical indicating whether to order the rows of the stacked ARDs, allowing
statistics that share common group and variable values to appear in
consecutive rows. Default is \code{FALSE}. Ordering will be based on the order
of the group/variable values prior to stacking.}

\item{.quiet}{(\code{logical})\cr
logical indicating whether to suppress any messaging. Default is \code{FALSE}}
}
\value{
an ARD data frame of class 'card'
}
\description{
Wrapper for \code{dplyr::bind_rows()} with additional checks
for duplicated statistics.
}
\examples{
ard <- ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")

bind_ard(ard, ard, .update = TRUE)
}
