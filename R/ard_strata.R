#' Stratified ARD
#'
#' @description
#' `r lifecycle::badge('experimental')`\cr
#' General function for calculating ARD results within subgroups.
#'
#' While the examples below show use with other functions from the cards package,
#' this function would primarily be used with the statistical functions in the
#' cardx functions.
#'
#' @param .data (`data.frame`)\cr
#'   a data frame
#' @param .by,.strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to tabulate by/stratify by for calculation.
#'   Arguments are similar, but with an important distinction:
#'
#'   `.by`: results are tabulated by **all combinations** of the columns specified,
#'       including unobserved combinations and unobserved factor levels.
#'
#'   `.strata`: results are tabulated by **all _observed_ combinations** of the
#'      columns specified.
#'
#'  These argument *should not* include any columns that appear in the `.f` argument.
#' @param .f (`function`, `formula`)\cr
#'   a function or a formula that can be coerced to a function with
#'   `rlang::as_function()` (similar to `purrr::map(.f)`)
#' @param ... Additional arguments passed on to the `.f` function.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_strata(
#'   ADSL,
#'   .by = ARM,
#'   .f = ~ ard_continuous(.x, variables = AGE)
#' )
ard_strata <- function(.data, .by = NULL, .strata = NULL, .f, ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(.data)
  check_not_missing(.f)
  check_data_frame(.data)

  # process inputs -------------------------------------------------------------
  .f <- rlang::as_function(x = .f, call = get_cli_abort_call())
  process_selectors(.data, .by = {{ .by }}, .strata = {{ .strata }})

  # nest the data frame --------------------------------------------------------
  df_nested_data <- nest_for_ard(.data, by = .by, strata = .strata)

  # run fn on nested data frames -----------------------------------------------
  df_nested_data <- df_nested_data |>
    dplyr::mutate(ard = map(.data$data, .f, ...)) |>
    dplyr::select(-"data")

  # rename grouping variables --------------------------------------------------
  # get the number grouping columns in the calculations
  max_group_n <-
    map(
      df_nested_data$ard,
      ~ dplyr::select(.x, all_ard_groups("names")) |> names()
    ) |>
    unlist() |>
    unique() |>
    sort() |>
    str_remove(pattern = "^group") |>
    as.integer() %>%
    # if no grouping variables are present, this will return `-Inf`
    {suppressWarnings(max(..1 = .))} # styler: off

  if (!is.infinite(max_group_n) && !is_empty(c(.by, .strata))) {
    new_group_colnames <-
      c(
        paste0("group", seq_along(c(.by, .strata)) + max_group_n),
        paste0("group", seq_along(c(.by, .strata)) + max_group_n, "_level")
      ) |>
      sort()
    names(df_nested_data)[seq_along(new_group_colnames)] <- new_group_colnames
  }

  # unnest ard data frame and return final table -------------------------------
  df_nested_data |>
    tidyr::unnest(cols = all_of("ard")) |>
    as_card() |>
    tidy_ard_column_order()
}
