#' ARD Total N
#'
#' Returns the total N for the data frame.
#' The placeholder variable name returned in the object is `"..ard_total_n.."`
#'
#' @inheritParams ard_dichotomous
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_total_n(ADSL)
ard_total_n <- function(data) {
  # process inputs -------------------------------------------------------------
  set_cli_abort_call()
  check_data_frame(data)

  # calculate total N ----------------------------------------------------------
  data |>
    dplyr::mutate(..ard_total_n.. = TRUE) |>
    ard_dichotomous(variables = "..ard_total_n..", statistic = list(..ard_total_n.. = "N")) |>
    dplyr::mutate(context = "total_n") |>
    dplyr::select(-all_ard_variables("levels"))
}
