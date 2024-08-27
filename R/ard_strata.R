#' Stratified ARD
#'
#' @description
#' General function for calculating ARD results within subgroups.
#'
#' While the examples below show use with other functions from the cards package,
#' this function would primarily be used with the statistical functions in the
#' cardx functions.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to stratify results by.
#' @param .ard_fn (`function`, `formula`)\cr
#'   a function or a formula that can be corced to a function with
#'   `rlang::as_function()` (similar to `purrr::map(.f)`)
#' @param ... Additional arguments passed on to the `.ard_fn` function.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_strata(
#'   ADSL,
#'   strata = ARM,
#'   ~ard_continuous(.x, variables = AGE)
#' )
#'
#' ard_strata(
#'   ADSL,
#'   strata = ARM,
#'   ard_continuous,
#'   variables = AGE
#' )
ard_strata <- function(data, strata, .ard_fn, ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(strata)
  check_not_missing(.ard_fn)
  check_data_frame(data)

  # process inputs -------------------------------------------------------------
  .ard_fn <- rlang::as_function(x = .ard_fn, call = get_cli_abort_call())
  process_selectors(data, strata = {{ strata }})
  if (is_empty(strata)) {
    cli::cli_abort("The {.arg strata} argument cannot be empty.", call = get_cli_abort_call())
  }

  # nest the data frame --------------------------------------------------------
  df_nested_data <- nest_for_ard(data, by = strata)

  # run fn on nested data frames -----------------------------------------------
  df_nested_data <- df_nested_data |>
    dplyr::mutate(ard = lapply(.data$data, .ard_fn, ...)) |>
    dplyr::select(-"data")

  # rename grouping variables --------------------------------------------------
  # get the number grouping columns in the calculations
  max_group_n <-
    map(
      df_nested_data$ard,
      ~dplyr::select(.x, all_ard_groups("names")) |> names()
    ) |>
    unlist() |>
    unique() |>
    sort() |>
    str_remove(pattern = "^group") |>
    as.integer() %>%
    # if no grouping variables are present, this will return `-Inf`
    {suppressWarnings(max(..1 = .))} # styler: off

  if (!is.infinite(max_group_n)) {
    new_group_colnames <-
      c(paste0("group", seq_along(strata) + 1L),
        paste0("group", seq_along(strata) + 1L, "_level")) |>
      sort()
    names(df_nested_data)[seq_along(new_group_colnames)] <- new_group_colnames
  }

  # unnest ard data frame and return final table -------------------------------
  df_nested_data |>
    tidyr::unnest(cols = all_of("ard")) |>
    as_card()
}
