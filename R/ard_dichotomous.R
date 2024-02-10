#' Dichotomous ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for dichotomous summary statistics.
#'
#' @inheritParams ard_categorical
#' @param values (named `list`)\cr
#'   named list of dichotomous values to tabulate. Default is `maximum_variable_values(data)`,
#'   which returns the largest/last value after a sort.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_dichotomous(mtcars, by = vs, variables = c(cyl, am), values = list(cyl = 4))
#'
#' mtcars |>
#'   dplyr::group_by(vs) |>
#'   ard_dichotomous(
#'     variables = c(cyl, am),
#'     values = list(cyl = 4),
#'     statistics = ~ categorical_variable_summary_fns("p")
#'   )
ard_dichotomous <- function(data,
                            variables,
                            by = dplyr::group_vars(data),
                            strata = NULL,
                            values = maximum_variable_values(data[variables]),
                            statistics = everything() ~ categorical_variable_summary_fns(),
                            denominator = NULL,
                            fmt_fn = NULL,
                            stat_labels = everything() ~ default_stat_labels()) {
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  check_class_data_frame(x = data)

  # process inputs -------------------------------------------------------------
  process_selectors(data, variables = {{ variables }})
  process_formula_selectors(data[variables], values = values)
  fill_formula_selectors(
    data = data[variables],
    values = formals(cards::ard_dichotomous)[["values"]] |> eval()
  )
  .check_dichotomous_values(data, values)

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
    statistics = statistics,
    denominator = denominator,
    fmt_fn = fmt_fn,
    stat_labels = stat_labels
  ) |>
    dplyr::filter(
      pmap(
        list(.data$variable, .data$variable_level),
        function(variable, variable_level) {
          variable_level %in% .env$values[[variable]]
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
#' This is used as the default value in `ard_dichotomous(values)` if not specified by
#' the user.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#'
#' @return a named list
#' @export
#'
#' @examples
#' ADSL[c("AGEGR1", "BMIBLGR1")] |> maximum_variable_values()
maximum_variable_values <- function(data) {
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
#' Check the validity of the values passed in `ard_dichotomous(values)`.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param values (named `list`)\cr
#'   a named list
#' @param call (`environment`)\cr
#'   frame for error messaging. Default is [parent.frame()].
#'
#' @return returns invisible if check is successful, throws an error message if not.
#' @keywords internal
#'
#' @examples
#' cards:::.check_dichotomous_values(mtcars, list(cyl = 4))
.check_dichotomous_values <- function(data, values, call = parent.frame()) {
  imap(
    values,
    function(value, column) {
      if (length(value) != 1L || is_empty(value) || is.na(value) || is.nan(value) || is.infinite(value)) {
        cli::cli_abort(c(
          "Error in argument {.arg values} for variable {.val {column}}.",
          "i" = "The length of the value must be one and not one of {.val {c(NA, NaN, Inf)}}."
        ), call = call)
      }
      if (inherits(data[[column]], "factor") && !value %in% levels(data[[column]])) {
        cli::cli_abort(c(
          "Error in argument {.arg values} for variable {.val {column}}.",
          "i" = "A value of {.val {value}} was passed, but must be one of {.val {levels(data[[column]])}}."
        ), call = call)
      } else if (!value %in% data[[column]]) {
        cli::cli_abort(c(
          "Error in argument {.arg values} for variable {.val {column}}.",
          "i" = "A value of {.val {value}} was passed, but must be one of {.val {unique(data[[column]]) |> na.omit() |> sort()}}."
        ), call = call)
      }
    }
  ) |>
    invisible()
}
