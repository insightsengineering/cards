#' Process tidyselectors
#'
#' @description
#' Functions processes tidyselect arguments passed to functions in the cards package.
#' The processed values saved to the calling environment, by default.
#'
#' - `process_selectors()`: the arguments will be processed with tidyselect and
#'   converted to a vector of character column names.
#'
#' - `process_formula_selectors()`: for arguments that expect named lists or
#'   lists of formulas (where the LHS of the formula is a tidyselector). This
#'   function processes these inputs and returns a named list. If an name is
#'   repeated, the last entry is kept.
#'
#' - `fill_formula_selectors()`: when users override the default argument values,
#'   it can be important to ensure that each column from a data frame is assigned
#'   a value. This function checks that each column in `data` has an assigned
#'   value, and if not, fills the value in with the default value passed here.
#'
#' - `compute_formula_selector()`: used in `process_formula_selectors()` to
#'   evaluate a single argument.
#'
#' - `check_list_elements()`: accepts named arguments where the name is a list
#'   that exists in the env, and the argument value is a predicate function
#'   used to the values of the list.
#'
#' @param data a data frame
#' @param ... named arguments where the value of the argument is processed with tidyselect.
#' - `process_selectors()`: the values are tidyselect-compatible selectors
#' - `process_formula_selectors()`: the values are named lists, list of formulas
#'   a combination of both, or a single formula. Users may pass `~value` as a
#'   shortcut for `everything() ~ value`.
#' - `check_list_elements()`: named arguments where the name matches an existing
#'   list in the `env` environment, and the value is a predicate function
#'   to test each element of the list, e.g. each element must be a string or
#'   a function.
#' @param env env to save the results to. Default is the calling environment.
#' @param x a named list, list of formulas, or a single formula that will be
#' converted to a named list.
#' @param arg_name a string with the argument named being processed. Used
#' in error messaging.
#' @param error_msg a named list where the list elements are strings that will
#' be used in error messaging when mis-specified arguments are passed. Elements
#' `"{arg_name}"` and `"{variable}"` are available using glue syntax for messaging.
#'
#' @name process_selectors
#'
#' @examples
#' example_env <- rlang::new_environment()
#'
#' process_selectors(ADSL, variables = starts_with("TRT"), env = example_env)
#' get(x = "variables", envir = example_env)
#'
#'
#' process_formula_selectors(
#'   ADSL,
#'   statistics = list(starts_with("TRT") ~ mean, TRTSDT = min),
#'   env = example_env
#' )
#' get(x = "statistics", envir = example_env)
#'
#' # process one list
#' compute_formula_selector(ADSL, x = starts_with("U") ~ 1L)
NULL

#' @name process_selectors
#' @export
process_selectors <- function(data, ..., env = rlang::caller_env()) {
  # saved dots as named list of quos
  dots <- rlang::enquos(...)

  # save named list of character column names selected
  ret <-
    lapply(
      dots,
      function(x) tidyselect::eval_select(x, data = data, allow_rename = FALSE) |> names()
    ) |>
    stats::setNames(names(dots))

  # save processed args to the calling env (well, that is the default env)
  for (i in seq_along(ret)) {
    assign(x = names(ret)[i], value = ret[[i]], envir = env)
  }
}


#' @name process_selectors
#' @export
process_formula_selectors <- function(data, ..., env = rlang::caller_env()) {
  # saved dots as named list
  dots <- rlang::dots_list(...)

  ret <-
    vector(mode = 'list', length = length(dots)) |>
    stats::setNames(nm = names(dots))
  for (i in seq_along(dots)) {
    ret[[i]] <-
      compute_formula_selector(data = data, x = dots[[i]],
                               arg_name = names(dots)[i], env = env)
  }

  # save processed args to the calling env (well, that is the default env)
  for (i in seq_along(ret)) {
    assign(x = names(ret)[i], value = ret[[i]], envir = env)
  }
}

#' @name process_selectors
#' @export
fill_formula_selectors <- function(data, ..., env = rlang::caller_env()) {
  dots <- rlang::dots_list(...)
  ret <- rlang::rep_named(names(dots), list(NULL))
  data_names <- names(data)
  dots_names <- names(dots)

  for (i in seq_along(dots)) {
    if (!rlang::is_empty(setdiff(data_names, names(get(dots_names[i], envir = env))))) {
      # process the default selector
      ret[[i]] <-
        compute_formula_selector(data = data, x = dots[[i]],
                                 arg_name = dots_names[i], env = env)
      # add the previously specified values and overwrite the default
      ret[[i]][names(get(dots_names[i], envir = env))] <-
        get(dots_names[i], envir = env)
    }
  }

  # save processed args to the calling env (well, that is the default env)
  for (i in seq_along(ret)) {
    if (!is.null(ret[[i]])) assign(x = names(ret)[i], value = ret[[i]], envir = env)
  }
}

#' @name process_selectors
#' @export
compute_formula_selector <- function(data, x, arg_name = rlang::caller_arg(x), env = rlang::caller_env()) {
  # user passed a named list, return unaltered
  if (.is_named_list(x)) return(x)

  # if user passed a single formula, wrap it in a list
  if (inherits(x, "formula")) x <- list(x)

  for(i in seq_along(x)) {
    # first check the class of the list element
    if (!.is_named_list(x[i]) && !inherits(x[[i]], "formula")) {
      "The {.arg {arg_name}} argument must be a named list, list of formulas, or a single formula." |>
      cli::cli_abort(call = env)
    }
    # if element is a formula, convert to a named list
    if (inherits(x[[i]], "formula")) {
      colnames <-
        eval(
          tidyselect::eval_select(
            # if nothing found on LHS of formula, using `everything()`
            rlang::f_lhs(x[[i]]) %||% dplyr::everything(),
            data = data
          ) |>
            names(),
          envir = attr(x[[i]], ".Environment")
        )
      x[i] <-
        rep_len(
          list(
            rlang::eval_tidy(rlang::f_rhs(x[[i]]), env = attr(x[[i]], ".Environment"))
            ),
          length.out = length(colnames)
        ) |>
        stats::setNames(nm = colnames) |>
        list()
    }
  }

  # flatten the list to a top-level list only
  x <- .purrr_list_flatten(x)

  # remove duplicates (keeping the last one)
  x[names(x) |> rev() |> Negate(duplicated)()]
}

#' @name process_selectors
#' @export
check_list_elements <- function(..., error_msg = list(), env = rlang::caller_env()) {
  dots <- rlang::dots_list(...)

  imap(
    dots,
    function(predicate_fn, arg_name) {
      imap(
        get(arg_name, envir = env),
        function(lst_element, variable) {
          if (!isTRUE(predicate_fn(lst_element))) {
            msg <-
              error_msg[[arg_name]] %||%
              "The value for argument {.arg {arg_name}} and variable {.val {variable}} is not the expected type."
            cli::cli_abort(message = msg)
          }
        }
      )
    }
  )

  invisible()
}

