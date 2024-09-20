#' Title
#'
#' @param data
#' @param hierarchies
#' @param by
#' @param id
#' @param denominator (`data.frame`)
#' @param include
#' @param overall
#' @param attributes
#' @param total_n
#' @param shuffle
#'
#' @return
#' @export
#'
#' @examples
ard_stack_hierarchical <- function(data,
                                   hierarchies,
                                   by = NULL,
                                   id = NULL,
                                   denominator = NULL,
                                   include = everything(),
                                   overall = FALSE,
                                   attributes = FALSE,
                                   total_n = FALSE,
                                   shuffle = FALSE) {
  set_cli_abort_call()

  # process inputs -------------------------------------------------------------
  cards::process_selectors(data, hierarchies = {{ hierarchies }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[hierarchies], include = {{ include }})

  # check inputs ---------------------------------------------------------------
  if (!is_empty(id)) check_data_frame(denominator) # styler: off
  if (!is.null(denominator) && is_empty(id)) {
    cli::cli_abort("The {.arg id} argument must be specified when the {.arg denominator} is specified.",
                   call = get_cli_abort_call())
  }

  # both hierarchies and include must be specified
  if (is_empty(hierarchies) || is_empty(include)) {
    cli::cli_abort(
      "Arguments {.arg hierarchies} and {.arg include} cannot be empty.",
      call = get_cli_abort_call()
    )
  }

  # the last hierarchies variable should be included
  if (!rev(hierarchies)[1] %in% include) {
    cli::cli_abort(
      "The last column specified in the {.arg hierarchies} ({.val {rev(hierarchies)[1]}}) must be in the {.arg include} argument.",
      call = get_cli_abort_call()
    )
  }

  if (is_empty(by) && isTRUE(overall)) {
    cli::cli_inform(
      c("The {.arg by} argument should be specified when using {.code total_noverall=TRUE}.",
        i = "Setting {.code ard_stack_hierarchical(overall=FALSE)}.")
    )
    overall <- FALSE
  }

  # drop missing values --------------------------------------------------------
  df_na_nan <- is.na(data[c(by, hierarchies)]) | apply(data[c(by, hierarchies)], MARGIN = 2, is.nan)
  if (any(df_na_nan)) {
    rows_with_na <- apply(df_na_nan, MARGIN = 1, any)
    cli::cli_inform(c("*" = "Removing {.val {sum(rows_with_na)}} row{?s} from {.arg data} with
                            {.val {NA}} or {.val {NaN}} values in {.val {c(by, hierarchies)}} column{?s}."))
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
                            {.val {NA}} or {.val {NaN}} values in {.val {c(by, hierarchies)}} column{?s}."))
      denominator <- denominator[!rows_with_na_denom, ]
    }
  }

  # sort data if using `ard_hierarchical(id)` ----------------------------------
  if (!is_empty(id)) data <- dplyr::arrange(data, dplyr::pick(all_of(c(id, by, hierarchies)))) # styler: off

  # go about calculating the statistics within the hierarchies -----------------
  # define index in `hierarchies` that also appear in `include`
  browser()
  which_include <- which(hierarchies %in% include)

  lst_results <- list()
  for (i in which_include) {
    lst_results <-
      lst_results |>
      append(
        .run_hierarchical_fun(
          data = data,
          variables = hierarchies[seq_len(i)],
          by = all_of(by),
          denominator = denominator,
          id = all_of(id)
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
            variables = hierarchies[seq_len(i)],
            by = all(setdiff(by, names(denominator))),
            denominator = denominator,
            id = all_of(id)
          ) |>
            list()
        )
    }
  }

  # add attributes if requested ------------------------------------------------
  if (isTRUE(attributes)) {
    lst_results <-
      lst_results |>
      append(
        ard_attributes(dplyr::select(data, any_of(c(by, hierarchies)))) |>
          list()
      )
  }

  # add total n if requested ---------------------------------------------------
  if (isTRUE(total_n)) {
    lst_results <-
      lst_results |>
      append(
        ard_total_n(denominator) |>
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

# this function calculates either the counts or the rates of teh events
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
