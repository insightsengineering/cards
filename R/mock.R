#' Mock ARDs
#'
#' `r lifecycle::badge('experimental')`\cr
#' Create empty ARDs used to create mock tables or table shells.
#' Where applicable, the formatting functions are set to return `'xx'` or `'xx.x'`.
#'
#' @param variables (`character` or named `list`)\cr
#'    a character vector of variable names for functions `mock_continuous()`,
#'    `mock_missing()`, and `mock_attributes()`.
#'
#'    a named list for functions `mock_categorical()` and `mock_dichotomous()`,
#'    where the list element is a vector of variable values. For
#'    `mock_dichotomous()`, only a single value is allowed for each variable.
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   a named list, a list of formulas, or a single formula where the list elements
#'   are character vectors of statistic names to appear in the ARD.
#' @param by (named `list`)\cr
#'   a named list where the list element is a vector of variable values.
#' @param label (named `list`)\cr
#'   named list of variable labels, e.g. `list(cyl = "No. Cylinders")`.
#'
#' @return an ARD data frame of class 'card'
#' @name mock
#'
#' @examples
#' mock_categorical(
#'   variables =
#'     list(
#'       AGEGR1 = factor(c("<65", "65-80", ">80"), levels = c("<65", "65-80", ">80"))
#'     ),
#'   by = list(TRTA = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
#' ) |>
#'   apply_fmt_fn()
#'
#' mock_continuous(
#'   variables = c("AGE", "BMIBL"),
#'   by = list(TRTA = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
#' ) |>
#'   # update the mock to report 'xx.xx' for standard deviations
#'   update_ard_fmt_fn(variables = c("AGE", "BMIBL"), stat_names = "sd", fmt_fn = \(x) "xx.xx") |>
#'   apply_fmt_fn()
NULL

#' @rdname mock
#' @export
mock_categorical <- function(variables,
                             statistic = everything() ~ c("n", "p", "N"),
                             by = NULL) {
  set_cli_abort_call()

  # check/process inputs -------------------------------------------------------
  check_named_list_and_vector_elements(variables)
  check_named_list_and_vector_elements(by)
  process_formula_selectors(
    data = .empty_data_frame(names(variables)),
    statistic = statistic
  )
  check_list_elements(
    x = statistic,
    predicate = \(x) is.character(x) && all(x %in% c("n", "p", "N")),
    error_msg = "The elements of the {.arg statistic} argument must be vector with one or more of {.val {c('n', 'p', 'N')}}."
  )

  # create ARD -----------------------------------------------------------------
  # build the ARD for the by variables
  ard_by <- .construct_by_variable_ard(by)

  # create ARD for the variables
  ard_variables <-
    dplyr::tibble(
      variable = names(.env$variables),
      variable_level = map(.data$variable, ~ as.list(.env$variables[[.x]]))
    ) |>
    tidyr::unnest(cols = "variable_level") |>
    dplyr::left_join(
      enframe(statistic, "variable", "stat_name"),
      by = "variable"
    ) |>
    tidyr::unnest(cols = "stat_name") |>
    .process_nested_list_as_df(
      arg = rep_named(names(variables), list(default_stat_labels())),
      new_column = "stat_label",
      unlist = TRUE
    ) |>
    dplyr::mutate(
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name),
      context = "categorical",
      stat = list(NULL),
      error = list(NULL),
      warning = list(NULL),
      fmt_fn = map(
        .data$stat_name,
        ~ ifelse(.x %in% c("n", "N", "N_obs", "N_miss", "N_nonmiss"), \(x) "xx", \(x) "xx.x")
      )
    )

  # merge the by ARD and the primary variable ARD ------------------------------
  merge(ard_by, ard_variables, by = NULL) |>
    as_card() |>
    tidy_ard_row_order() |>
    tidy_ard_column_order()
}

#' @rdname mock
#' @export
mock_continuous <- function(variables,
                            statistic = everything() ~ c(
                              "N", "mean", "sd", "median",
                              "p25", "p75", "min", "max"
                            ),
                            by = NULL) {
  set_cli_abort_call()

  # check/process inputs -------------------------------------------------------
  check_class(variables, "character")
  if (!is_empty(by)) check_named_list_and_vector_elements(by) # styler: off
  process_formula_selectors(
    data = data.frame(matrix(ncol = length(variables), nrow = 0)) |> stats::setNames(variables),
    statistic = statistic
  )
  check_list_elements(
    x = statistic,
    predicate = is.character,
    error_msg = "The elements of the {.arg statistic} argument must be {.cls character} vector of statistic names."
  )

  # create ARD -----------------------------------------------------------------
  # build the ARD for the by variables
  ard_by <- .construct_by_variable_ard(by)

  # create ARD for the variables
  ard_variables <-
    dplyr::tibble(
      variable = .env$variables,
      stat_name = map(.data$variable, ~ .env$statistic[[.x]])
    ) |>
    tidyr::unnest(cols = "stat_name") |>
    .process_nested_list_as_df(
      arg = rep_named(variables, list(default_stat_labels())),
      new_column = "stat_label",
      unlist = TRUE
    ) |>
    dplyr::mutate(
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name),
      context = "continuous",
      stat = list(NULL),
      error = list(NULL),
      warning = list(NULL),
      fmt_fn = map(
        .data$stat_name,
        ~ ifelse(.x %in% c("n", "N", "N_obs", "N_miss", "N_nonmiss"), \(x) "xx", \(x) "xx.x")
      )
    )

  # merge the by ARD and the primary variable ARD ------------------------------
  merge(ard_by, ard_variables, by = NULL) |>
    as_card() |>
    tidy_ard_row_order() |>
    tidy_ard_column_order()
}

#' @rdname mock
#' @export
mock_dichotomous <- function(variables,
                             statistic = everything() ~ c("n", "p", "N"),
                             by = NULL) {
  set_cli_abort_call()

  # check/process inputs -------------------------------------------------------
  check_named_list_and_vector_elements(variables)
  check_list_elements(
    x = variables,
    predicate = \(x) length(x) == 1L,
    error_msg = "The list values of {.arg variables} argument must be length {.val {1}}.",
  )

  mock_categorical(variables = variables, statistic = statistic, by = by) |>
    dplyr::mutate(context = "dichotomous")
}

#' @rdname mock
#' @export
mock_missing <- function(variables,
                         statistic = everything() ~ c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"),
                         by = NULL) {
  set_cli_abort_call()

  # check/process inputs -------------------------------------------------------
  check_class(variables, "character")
  process_formula_selectors(
    data = data.frame(matrix(ncol = length(variables), nrow = 0)) |> stats::setNames(variables),
    statistic = statistic
  )
  check_list_elements(
    x = statistic,
    predicate = \(x) is.character(x) && all(x %in% c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss")),
    error_msg = "The elements of the {.arg statistic} argument must be vector
                 with one or more of {.val {c('N_obs', 'N_miss', 'N_nonmiss', 'p_miss', 'p_nonmiss')}}."
  )

  # build ARD ------------------------------------------------------------------
  mock_continuous(variables = variables, statistic = statistic, by = by) |>
    dplyr::mutate(context = "missing")
}

#' @rdname mock
#' @export
mock_attributes <- function(label) {
  set_cli_abort_call()

  if (!is_named(label) || !is.list(label)) {
    cli::cli_abort(
      "The {.arg label} argument must be a named list.",
      call = get_cli_abort_call()
    )
  }

  ard_attributes(
    data = .empty_data_frame(names(label)),
    label = label
  )
}

#' @rdname mock
#' @export
mock_total_n <- function() {
  set_cli_abort_call()

  ard_total_n(data.frame()) |>
    dplyr::mutate(
      stat = list(NULL),
      fmt_fn = list(\(x) "xx")
    )
}

check_named_list_and_vector_elements <- function(
    x,
    message = "The {.arg {arg_name}} argument must be a named list, and each element a vector of values.",
    arg_name = rlang::caller_arg(x),
    call = get_cli_abort_call(),
    envir = rlang::current_env()) {
  # check input is a named list
  if (!is_empty(x) && (!is_named(x) || !is.list(x))) {
    cli::cli_abort(message = message, call = call, .envir = envir)
  }

  check_list_elements(
    x = x,
    predicate = \(x) is_vector(x) && !is.list(x),
    error_msg = message,
    arg_name = arg_name
  )
}

.empty_data_frame <- function(x) {
  data.frame(matrix(ncol = length(x), nrow = 0)) |> stats::setNames(x)
}

.construct_by_variable_ard <- function(by) {
  ard_by <- tidyr::expand_grid(!!!map(by, as.list))
  # rename the by variables
  for (i in seq_along(by)) {
    ard_by <- ard_by |>
      dplyr::mutate("group{i}" := names(by)[i]) |>
      dplyr::rename("group{i}_level" := glue::glue("{names(by)[i]}"))
  }
  ard_by
}
