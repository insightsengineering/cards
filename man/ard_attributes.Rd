% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard_attributes.R
\name{ard_attributes}
\alias{ard_attributes}
\alias{ard_attributes.data.frame}
\alias{ard_attributes.default}
\title{ARD Attributes}
\usage{
ard_attributes(data, ...)

\method{ard_attributes}{data.frame}(data, variables = everything(), label = NULL, ...)

\method{ard_attributes}{default}(data, ...)
}
\arguments{
\item{data}{(\code{data.frame})\cr
a data frame}

\item{...}{These dots are for future extensions and must be empty.}

\item{variables}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
variables to include}

\item{label}{(named \code{list})\cr
named list of variable labels, e.g. \code{list(cyl = "No. Cylinders")}.
Default is \code{NULL}}
}
\value{
an ARD data frame of class 'card'
}
\description{
Add variable attributes to an ARD data frame.
\itemize{
\item The \code{label} attribute will be added for all columns, and when no label
is specified and no label has been set for a column using the \verb{label=} argument,
the column name will be placed in the label statistic.
\item The \code{class} attribute will also be returned for all columns.
\item Any other attribute returned by \code{attributes()} will also be added, e.g. factor levels.
}
}
\examples{
df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
attr(df$var1, "label") <- "Lowercase Letters"

ard_attributes(df, variables = everything())
}
