% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_ard_conditions.R
\name{print_ard_conditions}
\alias{print_ard_conditions}
\title{Print ARD Condition Messages}
\usage{
print_ard_conditions(x)
}
\arguments{
\item{x}{(\code{data.frame})\cr
an ARD data frame of class 'card'}
}
\value{
returns invisible if check is successful, throws all condition messages if not.
}
\description{
Function parses the errors and warnings observed while calculating the
statistics requested in the ARD and prints them to the console as messages.
}
\examples{
# passing a character variable for numeric summary
ard_continuous(ADSL, variables = AGEGR1) |>
  print_ard_conditions()
}
