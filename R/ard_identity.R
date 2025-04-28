#' ARD Identity
#'
#' Function ingests pre-calculated statistics and returns the identical results,
#' but in an ARD format.
#'
#' @param x (named `list`)\cr
#'   named list of results. Names are the statistic names, and the values
#'   are the statistic values. These comprise the `"stat_name"` and `"stat"`
#'   columns in the returned ARD.
#' @param variable (`string`)\cr
#'   string of a variable name that is assigned to the `"variable"` column in the
#'   ARD.
#' @param context (`string`)\cr
#'   string to be added to the `"context"` column. Default is `"identity"`.
#'
#' @returns a ARD
#' @export
#'
#' @examples
#' t.test(formula = AGE ~ 1, data = ADSL)[c("statistic", "parameter", "p.value")] |>
#'   ard_identity(variable = "AGE", context = "ard_onesample_t_test")
ard_identity <- function(x, variable, context = "identity") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "list")
  check_named(x)
  check_string(variable)
  check_string(context)

  # build data frame for calculation -------------------------------------------
  dplyr::tibble("{variable}" := TRUE) |>
    ard_continuous(
      variables = all_of(variable),
      statistic = everything() ~ list(identity = \(xxx) x)
    ) |>
    dplyr::mutate(
      context = .env$context
    )
}
