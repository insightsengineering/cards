#' Build ARD from tidier
#'
#' Function converts a model's one-row tidy data frame into an ARD structure.
#' The tidied data frame must have been constructed with
#' `eval_capture_conditions()`.
#'
#' @param lst_tidy list of tidied results constructed with `eval_capture_conditions()`,
#' e.g. `eval_capture_conditions(t.test(mtcars$mpg ~ mtcars$am) |> broom::tidy())`
#' @param tidy_result_names character vector of column names expected from the
#' tidier method. This is used to construct blank results in the event of an error.
#' @param fun_args_to_record character vector of function argument names that
#' are added to the ARD.
#' @param formals the results from `formals()`, e.g. `formals(fisher.test)`.
#' This is used to get the default argument values from unspecified arguments.
#' @param passed_args named list of additional arguments passed to the modeling
#' function.
#' @param context a string added as the context column in the resulting ARD
#' @param lst_ard_columns named list of values that will be added to the ARD
#' data frame.
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
tidy_as_ard <- function(lst_tidy,
                        tidy_result_names,
                        fun_args_to_record,
                        formals = list(),
                        passed_args = list(),
                        context,
                        lst_ard_columns) {
  # used argument values -------------------------------------------------------
  lst_used_fun_args <-
    tryCatch(
      utils::modifyList(
        x = formals %||% list(),
        val = passed_args %||% list(),
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
    !!!lst_ard_columns,
    stat_name = names(lst_all_results),
    statistic = lst_all_results,
    context = .env$context,
    warning = lst_tidy["warning"],
    error = lst_tidy["error"]
  ) %>%
    structure(., class = c("card", class(.)))
}
