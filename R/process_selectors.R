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
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param ... ([`dynamic-dots`][dyn-dots])\cr
#'   named arguments where the value of the argument is processed with tidyselect.
#'   - `process_selectors()`: the values are tidyselect-compatible selectors
#'   - `process_formula_selectors()`: the values are named lists, list of formulas
#'     a combination of both, or a single formula. Users may pass `~value` as a
#'     shortcut for `everything() ~ value`.
#'   - `check_list_elements()`: named arguments where the name matches an existing
#'     list in the `env` environment, and the value is a predicate function
#'     to test each element of the list, e.g. each element must be a string or
#'     a function.
#' @param env (`environment`)\cr
#'   env to save the results to. Default is the calling environment.
#' @param x ([`formula-list-selector`][syntax])\cr
#'   a named list, list of formulas, or a single formula that will be
#'   converted to a named list.
#' @param arg_name (`string`)\cr
#'   a string with the argument named being processed. Used
#'   in error messaging. Default is `caller_arg(x)`
#' @param error_msg (`character`)\cr
#'   a named list where the list elements are strings that will
#'   be used in error messaging when mis-specified arguments are passed. Elements
#'   `"{arg_name}"` and `"{variable}"` are available using glue syntax for messaging.
#' @param strict (`logical` scalar)\cr
#'   whether to throw an error if a variable doesn't exist in the reference data
#'   (passed to `tidyselect::eval_select`)
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
process_selectors <- function(data, ..., env = caller_env()) {
  # saved dots as named list of quos
  dots <- enquos(...)

  # save named list of character column names selected
  ret <-
    imap(
      dots,
      function(x, arg_name) {
        processed_value <-
          eval_capture_conditions({
            tidyselect::eval_select(x, data = data, allow_rename = FALSE) |> names()
          })
        if (!is.null(processed_value[["result"]])) return(processed_value[["result"]])

        cli::cli_abort(
          c("There was an error selecting the {.arg {arg_name}} argument. See message below:",
            "i" = "{processed_value[['error']]}"),
          class = "process_selectors_error",
          call = env
        )
      }
    )

  # save processed args to the calling env (well, that is the default env)
  for (i in seq_along(ret)) {
    assign(x = names(ret)[i], value = ret[[i]], envir = env)
  }
}


#' @name process_selectors
#' @export
process_formula_selectors <- function(data, ..., env = caller_env()) {
  # saved dots as named list
  dots <- dots_list(...)

  # initialize empty list to store results and evaluate each input
  ret <- rep_named(names(dots), list())
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
fill_formula_selectors <- function(data, ..., env = caller_env()) {
  dots <- dots_list(...)
  ret <- rep_named(names(dots), list(NULL))
  data_names <- names(data)
  dots_names <- names(dots)

  for (i in seq_along(dots)) {
    if (!is_empty(setdiff(data_names, names(get(dots_names[i], envir = env))))) {
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
compute_formula_selector <- function(data, x, arg_name = caller_arg(x), env = caller_env(), strict = TRUE) {
  # user passed a named list, return unaltered
  if (.is_named_list(x)) return(x)

  # if user passed a single formula, wrap it in a list
  if (inherits(x, "formula")) x <- list(x)

  for(i in seq_along(x)) {
    # first check the class of the list element
    if (!.is_named_list(x[i]) && !inherits(x[[i]], "formula")) {
      c("The {.arg {arg_name}} argument must be a named list, list of formulas, or a single formula.",
        "i" = "Review {.help [?syntax](cards::syntax)} for examples and details.") |>
      cli::cli_abort(call = env)
    }
    # if element is a formula, convert to a named list
    if (inherits(x[[i]], "formula")) {

      lhs_expr <- f_lhs(x[[i]])

      if (!is.null(data)){
        lhs_expr <- tidyselect::eval_select(
          # if nothing found on LHS of formula, using `everything()`
          f_lhs(x[[i]]) %||% dplyr::everything(),
          data = data,
          strict = strict
        ) |>
          names()
      }

      colnames <-
        eval(
          lhs_expr,
          envir = attr(x[[i]], ".Environment")
        )
      x[i] <-
        rep_len(
          list(
            eval_tidy(f_rhs(x[[i]]), env = attr(x[[i]], ".Environment"))
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
  x[names(x) |> rev() |> Negate(duplicated)() |> rev()]
}

#' @name process_selectors
#' @export
check_list_elements <- function(..., error_msg = list(), env = caller_env()) {
  dots <- dots_list(...)

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

