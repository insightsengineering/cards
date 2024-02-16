#' Stack ARDs
#'
#' Stack multiple ARD calls sharing common input `data` and `by` variables.
#' Optionally incorporate additional information on represented variables (i.e.
#' big N's, missingness, attributes) and/or tidy for use in displays with
#' `shuffle_ard()`.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to tabulate by in the series of ARD function calls
#' @param ... ([`dynamic-dots`][dyn-dots])\cr
#'   Series of ARD function calls to be run and stacked
#' @param .missing (`logical`)\cr
#'   logical indicating whether to include the results of `ard_missing()` for all
#'   variables represented in the ARD. Default is `FALSE`.
#' @param .attributes (`logical`)\cr
#'   logical indicating whether to include the results of `ard_attributes()` for all
#'   variables represented in the ARD. Default is `FALSE`.
#' @param .shuffle (`logical`)\cr
#'   logical indicating whether to perform `shuffle_ard()` on the final result.
#'   Default is `FALSE`.
#'
#' @return  a transformed ARD data frame (of class 'card' if `.shuffle = FALSE`)
#'
#' @export
#' @examples
#'
#' ard_stack(
#'   data = ADSL,
#'   by = "ARM",
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE")
#' )
#'
#' ard_stack(
#'   data = ADSL,
#'   by = "ARM",
#'   ard_categorical(variables = "AGEGR1"),
#'   ard_continuous(variables = "AGE"),
#'   .shuffle = TRUE
#' )
#'
ard_stack <- function(data,
                      by = NULL,
                      ...,
                      .missing = FALSE,
                      .attributes = FALSE,
                      .shuffle = FALSE) {
  # capture quosures -----------------------------------------------------------
  dots <- enquos(...)

  # process arguments ----------------------------------------------------------
  process_selectors(
    data,
    by = {{ by }}
  )

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_data_frame(x = data)

  check_scalar_logical(.missing)
  check_scalar_logical(.attributes)
  check_scalar_logical(.shuffle)

  # evaluate the dots using common `data` and `by`
  ard_list <- lapply(
    dots,
    function(x) {
      x_rhs <- f_rhs(x)
      x_fn <- call_name(x_rhs)
      x_args <- call_args(x_rhs)
      do.call(x_fn, c(list(data = data, by = by), x_args), envir = attr(x, ".Environment"))
    }
  )

  # compute Ns by group / combine main calls
  if (!is_empty(by)) {
    ard_full <- bind_ard(
      ard_list,
      ard_categorical(
        data = data,
        variables = by,
        statistics = everything() ~ categorical_variable_summary_fns("N")
      )
    )
  } else {
    ard_full <- bind_ard(ard_list, .update = TRUE)
  }

  # get all variables represented
  variables <- unique(ard_full$variable)

  # missingness
  if (isTRUE(.missing)) {
    ard_full <- bind_ard(
      ard_full,
      ard_missing(data = data, variables = variables)
    )
  }

  # attributes
  if (isTRUE(.attributes)) {
    ard_full <- bind_ard(
      ard_full,
      ard_attributes(data = data, variables = variables)
    )
  }

  # order
  ard_full <- tidy_ard_row_order(ard_full)

  # shuffle
  if (isTRUE(.shuffle)) {
    return(shuffle_ard(ard_full))
  }

  ard_full
}
