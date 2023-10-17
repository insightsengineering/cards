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
#' - `compute_formula_selector()`:
#'
#' @param data a data frame
#' @param ... named arguments where the value of the argument is processed with tidyselect.
#' - `process_selectors()`: the values are tidyselect-compatible selectors
#' - `process_formula_selectors()`: the values are named lists, list of formulas
#'   a combination of both, or a single formula. Users may pass `~value` as a
#'   shortcut for `everything() ~ value`.
#' @param env env to save the results to. Default is the calling environment.
#' @param x a named list, list of formulas, or a single formula that will be
#' converted to a named list.
#' @param arg_name a string with the argument named being processed. Used
#' in error messaging.
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
compute_formula_selector <- function(data, x, arg_name = '', env = rlang::caller_env()) {
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


