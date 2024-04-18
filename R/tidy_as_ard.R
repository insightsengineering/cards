#' Build ARD from Tidier
#'
#' @description
#' Function converts a model's one-row tidy data frame into an ARD structure.
#' The tidied data frame must have been constructed with
#' [eval_capture_conditions()].
#'
#' This function is primarily for developers and few consistency checks have
#' been included.
#'
#' @param lst_tidy (named `list`)\cr
#'   list of tidied results constructed with [eval_capture_conditions()],
#'   e.g. `eval_capture_conditions(t.test(mtcars$mpg ~ mtcars$am) |> broom::tidy())`.
#' @param tidy_result_names (`character`)\cr
#'   character vector of column names expected by the
#'   tidier method. This is used to construct blank results in the event of an error.
#' @param fun_args_to_record (`character`)\cr
#'   character vector of function argument names that are added to the ARD.
#' @param formals (`pairlist`)\cr
#'   the results from `formals()`, e.g. `formals(fisher.test)`.
#'   This is used to get the default argument values from unspecified arguments.
#' @param passed_args (named `list`)\cr
#'   named list of additional arguments passed to the modeling function.
#' @param lst_ard_columns (named `list`)\cr
#'   named list of values that will be added to the ARD data frame.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' # example how one may create a fisher.test() ARD function
#' my_ard_fishertest <- function(data, by, variable, ...) {
#'   # perform fisher test and format results -----------------------------------
#'   lst_tidy_fisher <-
#'     eval_capture_conditions(
#'       # this manipulation is similar to `fisher.test(...) |> broom::tidy()`
#'       stats::fisher.test(x = data[[variable]], y = data[[by]], ...)[c("p.value", "method")] |>
#'         as.data.frame()
#'     )
#'
#'   # build ARD ------------------------------------------------------------------
#'   tidy_as_ard(
#'     lst_tidy = lst_tidy_fisher,
#'     tidy_result_names = c("p.value", "method"),
#'     fun_args_to_record =
#'       c(
#'         "workspace", "hybrid", "hybridPars", "control", "or",
#'         "conf.int", "conf.level", "simulate.p.value", "B"
#'       ),
#'     formals = formals(stats::fisher.test),
#'     passed_args = dots_list(...),
#'     lst_ard_columns = list(group1 = by, variable = variable, context = "fishertest")
#'   )
#' }
#'
#' my_ard_fishertest(mtcars, by = "am", variable = "vs")
tidy_as_ard <- function(lst_tidy,
                        tidy_result_names,
                        fun_args_to_record = character(0L),
                        formals = list(),
                        passed_args = list(),
                        lst_ard_columns) {
  set_cli_abort_call()

  # used argument values -------------------------------------------------------
  lst_used_fun_args <-
    tryCatch(
      utils::modifyList(
        x =
        # missing() is TRUE if the arg is not specified,
        # not actually missing (ie it can still have its default value)
          if (missing(formals)) formals else formals[fun_args_to_record],
        val = passed_args,
        keep.null = TRUE
      ),
      error = function(e) list()
    )

  # if there are results, put them in the ARD format ---------------------------
  if (!is.null(lst_tidy[["result"]])) {
    # combine results and function argument
    lst_all_results <-
      c(unclass(lst_tidy[["result"]]), lst_used_fun_args)
  }
  # if there was an error calculating results, tidy up what we can -------------
  else {
    # combine empty results and function arguments
    lst_all_results <-
      utils::modifyList(
        x =
          rep_len(
            x = list(NULL),
            length.out = length(c(tidy_result_names, fun_args_to_record))
          ) |>
            stats::setNames(nm = c(tidy_result_names, fun_args_to_record)),
        val = lst_used_fun_args,
        keep.null = TRUE
      )
  }

  # add results to tibble ------------------------------------------------------
  dplyr::tibble(
    stat_name = names(lst_all_results),
    stat = lst_all_results,
    fmt_fn = lapply(.data$stat, function(x) {
      switch(is.numeric(x),
        1L
      )
    }),
    warning = lst_tidy["warning"],
    error = lst_tidy["error"],
    !!!lst_ard_columns,
  ) |>
    tidy_ard_column_order() %>%
    {structure(., class = c("card", class(.)))} # styler: off
}
