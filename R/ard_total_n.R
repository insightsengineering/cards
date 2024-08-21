#' ARD Total N
#'
#' Returns the total N for the data frame.
#' The placeholder variable name returned in the object is `"..ard_total_n.."`
#'
#' @inheritParams ard_dichotomous
#' @inheritParams rlang::args_dots_empty
#'
#' @return an ARD data frame of class 'card'
#' @name ard_total_n
#'
#' @examples
#' ard_total_n(ADSL)
NULL

#' @rdname ard_total_n
#' @export
ard_total_n <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_total_n")
}

#' @rdname ard_total_n
#' @export
ard_total_n.data.frame <- function(data, ...) {
  # process inputs -------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty()
  check_data_frame(data)

  # calculate total N ----------------------------------------------------------
  data |>
    dplyr::mutate(..ard_total_n.. = TRUE) |>
    ard_dichotomous(variables = "..ard_total_n..", statistic = list(..ard_total_n.. = "N")) |>
    dplyr::mutate(context = "total_n") |>
    dplyr::select(-all_ard_variables("levels"))
}
