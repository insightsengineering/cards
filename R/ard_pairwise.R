#' Pairwise ARD
#'
#' Utility to perform pairwise comparisons.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column to perform pairwise analyses for.
#' @param .f (`function`)\cr
#'   a function that creates ARDs. The function accepts a single argument and
#'   a subset of `data` will be passed including the two levels of `variable`
#'   for the pairwise analysis.
#' @param include (`vector`)\cr
#'   a vector of levels of the `variable` column to include in comparisons.
#'   Pairwise comparisons will only be performed for pairs that have a level
#'   specified here. Default is `NULL` and all pairwise computations are included.
#'
#' @return list of ARDs
#' @export
#'
#' @examples
#' ard_pairwise(
#'   ADSL,
#'   variable = ARM,
#'   .f = \(df) {
#'     ard_complex(
#'       df,
#'       variables = AGE,
#'       statistic = ~ list(ttest = \(x, data, ...) t.test(x ~ data$ARM)[c("statistic", "p.value")])
#'     )
#'   },
#'   include = "Placebo" # only include comparisons to the "Placebo" group
#' )
ard_pairwise <- function(data, variable, .f, include = NULL) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_data_frame(data)
  process_selectors(data, variable = {{ variable }})
  check_scalar(variable)
  if (!is_empty(include) && (!is_vector(include) || is.list(include))) {
    cli::cli_abort(
      "The {.arg include} argument must be a simple vector, not {.obj_type_friendly {include}}.",
      call = get_cli_abort_call()
    )
  }
  .f <- as_function(.f, call = get_cli_abort_call())
  variable_levels <- .unique_and_sorted(data[[variable]])
  if (!is_empty(include)) {
    if (!all(include %in% variable_levels)) {
      cli::cli_abort(
        "The {.arg include} argument must be NULL or one or more of {.val {variable_levels}}.",
        call = get_cli_abort_call()
      )
    }
  }
  include <- include %||% variable_levels # if include not specified, default to all levels

  # identify all pairwise values in `variable` ---------------------------------
  mtx_pairs <- variable_levels |> utils::combn(m = 2)
  lst_pairs <- seq_len(ncol(mtx_pairs)) |> lapply(FUN = \(x) mtx_pairs[, x])
  lst_pairs <- lst_pairs[map_lgl(lst_pairs, ~ any(.x %in% include))] # exclude pairs that were not requested

  # create data subsets including the pairs ------------------------------------
  lst_df_subsets <-
    lapply(
      lst_pairs,
      FUN = \(x) {
        df_subset <- data |> dplyr::filter(.data[[variable]] %in% .env$x)
        if (is.factor(data[[variable]])) {
          data[[variable]] <- factor(data[[variable]], ordered = is.ordered(data[[variable]]))
        }
        df_subset
      }
    ) |>
    # set names for returned list including the pair levels
    stats::setNames(map_chr(lst_pairs, ~ as.character(.x) |>
      shQuote(type = "csh") |>
      paste(collapse = " vs. ")))

  # perform analysis -----------------------------------------------------------
  lst_ard <-
    imap(
      lst_df_subsets,
      \(df, pairs) {
        eval_capture_conditions(.f(df)) |>
          captured_condition_as_error(
            message = c(glue::glue("The following {{type}} occurred for {pairs}. See message below."), x = "{condition}")
          )
      }
    )

  # return result --------------------------------------------------------------
  lst_ard
}
