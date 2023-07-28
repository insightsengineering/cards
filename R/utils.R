#' Convert Data Frame to Named List
#'
#' Function takes a two column data frame as inputs.
#' The first column will be the names of the resulting list, and the second
#' column the values of the list.
#'
#' @param x a data frame
#'
#' @return a named list
#' @noRd
.data_frame_to_named_list <- function(x) {
  as.list(x[[2]]) |> stats::setNames(nm = x[[1]])
}


.get_ard_label_statistic <- function(ard, variable = NULL) {
  # subset by variable name if supplied
  if (!is.null(variable)) {
    ard <- dplyr::filter(ard, .data$variable %in% .env$variable)
  }

  # filter on the label statistic
  ard <- dplyr::filter(ard, .data$stat_name %in% 'label')

  # select the label and return. If empty, NULL is returned
  unlist(ard$statistic)
}
