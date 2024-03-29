% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shuffle_ard.R
\name{.fill_grps_from_variables}
\alias{.fill_grps_from_variables}
\title{Back Fill Group Variables}
\usage{
.fill_grps_from_variables(x)
}
\arguments{
\item{x}{(\code{data.frame})\cr
a data frame}
}
\value{
data frame
}
\description{
This function back fills the values of group variables using
variable/variable_levels. The back filling will occur if the value of the
\code{variable} column matches the name of a grouping variable, and the grouping
variable's value is \code{NA}.
}
\examples{
data <- data.frame(
  variable = c(rep("A", 3), rep("B", 2)),
  variable_level = 1:5,
  A = rep(NA, 5),
  B = rep(NA, 5)
)

cards:::.fill_grps_from_variables(data)
}
\keyword{internal}
