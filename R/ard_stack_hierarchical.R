#' Stacked Hierarchical ARD Statistics
#'
#' @description
#' `r lifecycle::badge('experimental')`\cr
#' Use this function to calculate multiple summaries of nested or hierarchical data
#' in a single call.
#'
#' When the `id` argument is specified, event rates and counts are calculated via
#' `ard_hierarchical()`; otherwise, counts are calculated using `ard_hierarchical_count()`.
#' See below for details on rate calculations when using the `id` argument.
#'
#' @details
#' ADD DETAILS ABOUT HOW THE DATA ARE SORTED AND SUBSETTED.
#'
#'
#' @inheritParams ard_hierarchical
#' @inheritParams ard_stack
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Specifies the nested/hierarchical structure of the data.
#'   The variables that are specified here and in the `include` argument
#'   will have summary statistics calculated.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   an optional argument used to subset `data` to obtain the correct percentages.
#'   When specified, `ard_hierarchical()` is used to calculate event rates. When
#'   not specified, counts are returned via `ard_hierarchical_count()`.
#' @param denominator (`data.frame`, `integer`)\cr
#'   an optional argument used to define the denominator and enhance the output.
#'   - the univariate tabulations of the `by` variables are calculated with `denominator`,
#'     when a data frame is passed, e.g. tabulation of the treatment assignment
#'     counts that may appear in the header of a table.
#'   - the `denominator` argument must be specified when `id` is used to
#'     calculate the event rates.
#'   - if `total_n=TRUE`, the `denominator` argument is used to return the total N
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Specify the subset a columns indicated in the `variables` argument for which
#'   summary statistics will be returned. Default is `everything()`.
#' @param overall (scalar `logical`)\cr logical indicating whether overall statistics
#'   should be calculated (i.e. re-run all `ard_*()` calls with `by=NULL`).
#'   Default is `FALSE`.
#' @param overall_row (scalar `logical`)\cr logical indicating whether overall statistics
#'   should be calculated across the columns listed in the `variables` argument.
#'   Default is `FALSE`.
#' @param attributes (scalar `logical`)\cr
#'   logical indicating whether to include the results of `ard_attributes()` for all
#'   variables represented in the ARD. Default is `FALSE`.
#' @param total_n (scalar `logical`)\cr
#'   logical indicating whether to include of `ard_total_n(denominator)` in the returned ARD.
#' @param shuffle (scalar `logical`)\cr
#'   logical indicating whether to perform `shuffle_ard()` on the final result.
#'   Default is `FALSE`.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_stack_hierarchical(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   id = USUBJID
#' )
ard_stack_hierarchical <- function(data,
                                   variables,
                                   by = dplyr::group_vars(data),
                                   denominator = NULL,
                                   id = NULL,
                                   include = everything(),
                                   overall = FALSE,
                                   overall_row = FALSE,
                                   attributes = FALSE,
                                   total_n = FALSE,
                                   shuffle = FALSE) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data, variables = {{ variables }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[variables], include = {{ include }})
  check_scalar_logical(overall)
  check_scalar_logical(overall_row)
  check_scalar_logical(attributes)
  check_scalar_logical(total_n)
  check_scalar_logical(shuffle)

  # check inputs ---------------------------------------------------------------
  # denominator must be empty, a data frame, or integer
  if (!is_empty(denominator) && !is.data.frame(denominator) && !is_integerish(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} argument must be empty, a {.cls data.frame}, or an {.cls integer}, not {.obj_type_friendly {denominator}}.",
      call = get_cli_abort_call()
    )
  }

  # we use the denom arg to calculate rates
  if (!is_empty(id) && is_empty(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} must be specified when the {.arg id} argument is specified.",
      call = get_cli_abort_call()
    )
  }


  # both variables and include must be specified
  if (is_empty(variables) || is_empty(include)) {
    cli::cli_abort(
      "Arguments {.arg variables} and {.arg include} cannot be empty.",
      call = get_cli_abort_call()
    )
  }

  # the last `variables` variable should be included
  if (!utils::tail(variables, 1L) %in% include) {
    cli::cli_abort(
      "The last column specified in the {.arg variables} ({.val {utils::tail(variables, 1L)}}) must be in the {.arg include} argument.",
      call = get_cli_abort_call()
    )
  }

  if (is_empty(by) && isTRUE(overall)) {
    cli::cli_inform(
      c("The {.arg by} argument must be specified when using {.code overall=TRUE}.",
        i = "Setting {.code ard_stack_hierarchical(overall=FALSE)}.")
    )
    overall <- FALSE
  }

  if (is_empty(denominator) && isTRUE(total_n)) {
    cli::cli_inform(
      c("The {.arg denominator} argument must be specified when using {.code total_n=TRUE}.",
        i = "Setting {.code ard_stack_hierarchical(total_n=FALSE)}.")
    )
    total_n <- FALSE
  }

  # drop missing values --------------------------------------------------------
  df_na_nan <- is.na(data[c(by, variables)]) | apply(data[c(by, variables)], MARGIN = 2, is.nan)
  if (any(df_na_nan)) {
    rows_with_na <- apply(df_na_nan, MARGIN = 1, any)
    cli::cli_inform(c("*" = "Removing {.val {sum(rows_with_na)}} row{?s} from {.arg data} with
                            {.val {NA}} or {.val {NaN}} values in {.val {c(by, variables)}} column{?s}."))
    data <- data[!rows_with_na, ]
  }

  # remove missing by variables from `denominator`
  if (is.data.frame(denominator) && !is_empty(intersect(by, names(denominator)))) {
    df_na_nan_denom <-
      is.na(denominator[intersect(by, names(denominator))]) |
      apply(denominator[intersect(by, names(denominator))], MARGIN = 2, is.nan)
    if (any(df_na_nan_denom)) {
      rows_with_na_denom <- apply(df_na_nan_denom, MARGIN = 1, any)
      cli::cli_inform(c("*" = "Removing {.val {sum(rows_with_na_denom)}} row{?s} from {.arg denominator} with
                            {.val {NA}} or {.val {NaN}} values in {.val {c(by, variables)}} column{?s}."))
      denominator <- denominator[!rows_with_na_denom, ]
    }
  }

  # sort data if using `ard_hierarchical(id)` ----------------------------------
  if (!is_empty(id)) data <- dplyr::arrange(data, dplyr::pick(all_of(c(id, by, variables)))) # styler: off

  # go about calculating the statistics within the variables -------------------
  # define index in `variables` that also appear in `include`
  which_include <- which(variables %in% include)

  lst_results <- list()
  for (i in which_include) {
    lst_results <-
      lst_results |>
      append(
        .run_hierarchical_fun(
          data = data,
          variables = variables[seq_len(i)],
          by = by,
          denominator = denominator,
          id = id
        ) |>
          list()
      )
  }

  # calculate results overall if requested -------------------------------------
  if (isTRUE(overall)) {
    for (i in which_include) {
      lst_results <-
        lst_results |>
        append(
          .run_hierarchical_fun(
            data = data,
            variables = variables[seq_len(i)],
            by = setdiff(by, names(denominator)),
            denominator = denominator,
            id = id
          ) |>
            list()
        )
    }
  }

  # add univariate tabulations of by variables ---------------------------------
  if (is.data.frame(denominator) && !is_empty(intersect(by, names(denominator)))) {
    lst_results <-
      lst_results |>
      append(
        ard_categorical(
          data = denominator,
          variables = all_of(intersect(by, names(denominator)))
        ) |>
          list()
      )
  }

  # add overall row if requested -----------------------------------------------
  if (isTRUE(overall_row)) {
    lst_results <-
      lst_results |>
      append(
        .run_hierarchical_fun(
          data = data |> dplyr::mutate(..ard_hierarchical_overall.. = TRUE),
          variables = "..ard_hierarchical_overall..",
          by = by,
          denominator = denominator,
          id = id
        ) |>
          list()
      )
  }

  # add attributes if requested ------------------------------------------------
  if (isTRUE(attributes)) {
    lst_results <-
      lst_results |>
      append(
        ard_attributes(dplyr::select(data, any_of(c(by, variables)))) |>
          list()
      )
  }

  # add total n if requested ---------------------------------------------------
  if (isTRUE(total_n) && is.data.frame(denominator)) {
    lst_results <-
      lst_results |>
      append(ard_total_n(denominator) |> list())
  }
  else if (isTRUE(total_n) && is_integerish(denominator)) {
    lst_results <-
      lst_results |>
      append(
        ard_total_n(data) |>
          dplyr::mutate(stat = list(as.integer(denominator))) |>
          list()
      )
  }

  # combine results ------------------------------------------------------------
  result <- lst_results |>
    dplyr::bind_rows() |>
    cards::tidy_ard_column_order() |>
    cards::tidy_ard_row_order()

  # shuffle if requested -------------------------------------------------------
  if (isTRUE(shuffle)) {
    result <- shuffle_ard(result)
  }

  # return final result --------------------------------------------------------
  result
}

# this function calculates either the counts or the rates of the events
.run_hierarchical_fun <- function(data, variables, by, denominator, id) {
  if (is_empty(id)) {
    ard_hierarchical_count(
      data = data,
      variables = all_of(variables),
      by = all_of(by)
    )
  }
  else {
    ard_hierarchical(
      data = data |>
        dplyr::slice_tail(n = 1L, by = all_of(c(id, intersect(by, names(denominator)), variables))),
      variables = all_of(variables),
      by = all_of(by),
      denominator = denominator,
      id = all_of(id)
    )
  }
}
