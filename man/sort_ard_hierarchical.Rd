% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_ard_hierarchical.R
\name{sort_ard_hierarchical}
\alias{sort_ard_hierarchical}
\title{Sort Stacked Hierarchical ARDs}
\usage{
sort_ard_hierarchical(x, sort = c("descending", "alphanumeric"))
}
\arguments{
\item{x}{(\code{card})\cr
a stacked hierarchical ARD of class \code{'card'} created using \code{\link[=ard_stack_hierarchical]{ard_stack_hierarchical()}} or
\code{\link[=ard_stack_hierarchical_count]{ard_stack_hierarchical_count()}}.}

\item{sort}{(\code{string})\cr
type of sorting to perform. Value must be one of:
\itemize{
\item \code{"alphanumeric"} - within each hierarchical section of the ARD, groups are ordered alphanumerically (i.e. A to Z)
by \code{variable_level} text.
\item \code{"descending"} - within each variable group of the ARD, count sums are calculated for each group and groups are
sorted in descending order by sum. If \code{sort = "descending"}, the \code{n} statistic is used to calculate variable
group sums if included in \code{statistic} for all variables, otherwise \code{p} is used. If neither \code{n} nor \code{p} are
present in \code{x} for all variables, an error will occur.
}

Defaults to \code{"descending"}.}
}
\value{
an ARD data frame of class 'card'
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}\cr

This function is used to sort stacked hierarchical ARDs.

For the purposes of this function, we define a "variable group" as a combination of ARD rows grouped by the
combination of all their variable levels, but excluding any \code{by} variables.
}
\note{
If overall data is present in \code{x} (i.e. the ARD was created with \code{ard_stack_hierarchical(overall=TRUE)}), the
overall data will be sorted last within each variable group (i.e. after any other rows with the same combination of
variable levels).
}
\examples{
\dontshow{if ((identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
ard_stack_hierarchical(
  ADAE,
  variables = c(AESOC, AEDECOD),
  by = TRTA,
  denominator = ADSL,
  id = USUBJID
) |>
  sort_ard_hierarchical("alphanumeric")

ard_stack_hierarchical_count(
  ADAE,
  variables = c(AESOC, AEDECOD),
  by = TRTA,
  denominator = ADSL
) |>
  sort_ard_hierarchical("descending")
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=filter_ard_hierarchical]{filter_ard_hierarchical()}}
}
