#' Dichotomous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for dichotomous summary statistics.
#'
#' @inheritParams ard_categorical
#' @param value (named `list`)\cr
#'   named list of dichotomous values to tabulate. Default is `maximum_variable_value(data)`,
#'   which returns the largest/last value after a sort.
#'
#' @return an ARD data frame of class 'card'
#' @name ard_dichotomous
#'
#' @examples
#' ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), value = list(cyl = 4))
#'
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   ard_dichotomous(
#'     variables = c(cyl, am),
#'     value = list(cyl = 4),
#'     statistic = ~ "p"
#'   )
NULL

#' @rdname ard_dichotomous
#' @export
ard_dichotomous <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_dichotomous")
}

#' @rdname ard_dichotomous
#' @export
ard_dichotomous.data.frame <- function(data,
                                       variables,
                                       by = dplyr::group_vars(data),
                                       strata = NULL,
                                       value = maximum_variable_value(data[variables]),
                                       statistic = everything() ~ c("n", "N", "p"),
                                       denominator = NULL,
                                       fmt_fn = NULL,
                                       stat_label = everything() ~ default_stat_labels(),
                                       ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(variables)

  # process inputs -------------------------------------------------------------
  process_selectors(data, variables = {{ variables }})
  process_formula_selectors(data[variables], value = value)
  fill_formula_selectors(
    data[variables],
    value = formals(asNamespace("cards")[["ard_dichotomous.data.frame"]])[["value"]] |> eval()
  )
  .check_dichotomous_value(data, value)

  # return empty tibble if no variables selected -------------------------------
  if (is_empty(variables)) {
    return(dplyr::tibble())
  }

  # calculate summary statistics -----------------------------------------------
  ard_categorical(
    data = data,
    variables = all_of(variables),
    by = {{ by }},
    strata = {{ strata }},
    statistic = statistic,
    denominator = denominator,
    fmt_fn = fmt_fn,
    stat_label = stat_label
  ) |>
    dplyr::filter(
      pmap(
        list(.data$variable, .data$variable_level),
        function(variable, variable_level) {
          variable_level %in% .env$value[[variable]]
        }
      ) |>
        unlist()
    ) |>
    dplyr::mutate(context = "dichotomous")
}

#' Maximum Value
#'
#' For each column in the passed data frame, the function returns a named list
#' with the value being the largest/last element after a sort.
#' For factors, the last level is returned, and for logical vectors `TRUE` is returned.
#' This is used as the default value in `ard_dichotomous(value)` if not specified by
#' the user.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#'
#' @return a named list
#' @export
#'
#' @examples
#' ADSL[c("AGEGR1", "BMIBLGR1")] |> maximum_variable_value()
maximum_variable_value <- function(data) {
  data |>
    lapply(
      function(x) {
        if (inherits(x, "factor")) {
          return(levels(x) |> dplyr::last())
        }
        if (inherits(x, "logical")) {
          return(TRUE)
        }
        stats::na.omit(x) |>
          unique() |>
          sort() |>
          dplyr::last()
      }
    )
}


#' Perform Value Checks
#'
#' Check the validity of the values passed in `ard_dichotomous(value)`.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param value (named `list`)\cr
#'   a named list
#'
#' @return returns invisible if check is successful, throws an error message if not.
#' @keywords internal
#'
#' @examples
#' cards:::.check_dichotomous_value(mtcars, list(cyl = 4))
.check_dichotomous_value <- function(data, value) {
  imap(
    value,
    function(value, column) {
      accepted_values <- .unique_and_sorted(data[[column]])
      if (length(value) != 1L || !value %in% accepted_values) {
        message <- "Error in argument {.arg value} for variable {.val {column}}."
        cli::cli_abort(
          if (length(value) != 1L) {
            c(message, "i" = "The value must be one of {.val {accepted_values}}.")
          } else {
            c(message, "i" = "A value of {.val {value}} was passed, but must be one of {.val {accepted_values}}.")
          },
          call = get_cli_abort_call()
        )
      }
    }
  ) |>
    invisible()
}
