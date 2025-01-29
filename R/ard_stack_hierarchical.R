#' Stacked Hierarchical ARD Statistics
#'
#' @description
#' `r lifecycle::badge('experimental')`\cr
#' Use these functions to calculate multiple summaries of nested or hierarchical data
#' in a single call.
#'
#' - `ard_stack_hierarchical()`: Calculates *rates* of events (e.g. adverse events)
#'   utilizing the `denominator` and `id` arguments to identify the rows in `data`
#'   to include in each rate calculation.
#'
#' - `ard_stack_hierarchical_count()`: Calculates *counts* of events utilizing
#'   all rows for each tabulation.
#'
#' @section Subsetting Data for Rate Calculations:
#'
#' To calculate event rates, the `ard_stack_hierarchical()` function identifies
#' rows to include in the calculation.
#' First, the primary data frame is sorted by the columns identified in
#' the `id`, `by`, and `variables` arguments.
#'
#' As the function cycles over the variables specified in the `variables` argument,
#' the data frame is grouped by `id`, `intersect(by, names(denominator))`, and `variables`
#' utilizing the last row within each of the groups.
#'
#' For example, if the call is
#' `ard_stack_hierarchical(data = ADAE, variables = c(AESOC, AEDECOD), id = USUBJID)`,
#' then we'd first subset ADAE to be one row within the grouping `c(USUBJID, AESOC, AEDECOD)`
#' to calculate the event rates in `'AEDECOD'`. We'd then repeat and
#' subset ADAE to be one row within the grouping `c(USUBJID, AESOC)`
#' to calculate the event rates in `'AESOC'`.
#'
#' @section Overall Argument:
#' When we set `overall=TRUE`, we wish to re-run our calculations removing the
#' stratifying columns. For example, if we ran the code below, we results would
#' include results with the code chunk being re-run with `by=NULL`.
#'
#' ```r
#' ard_stack_hierarchical(
#'   data = ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   overall = TRUE
#' )
#' ```
#'
#' But there is another case to be aware of: when the `by` argument includes
#' columns that are not present in the `denominator`, for example when tabulating
#' results by AE grade or severity in addition to treatment assignment.
#' In the example below, we're tabulating results by treatment assignment and
#' AE severity. By specifying `overall=TRUE`, we will re-run the to get
#' results with `by = AESEV` and again with `by = NULL`.
#'
#' ```r
#' ard_stack_hierarchical(
#'   data = ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = c(TRTA, AESEV),
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   overall = TRUE
#' )
#' ```
#'
#' @inheritParams ard_hierarchical
#' @inheritParams ard_stack
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Specifies the nested/hierarchical structure of the data.
#'   The variables that are specified here and in the `include` argument
#'   will have summary statistics calculated.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   argument used to subset `data` to identify rows in `data` to calculate
#'   event rates in `ard_stack_hierarchical()`. See details below.
#' @param denominator (`data.frame`, `integer`)\cr
#'   used to define the denominator and enhance the output.
#'   The argument is required for `ard_stack_hierarchical()` and optional
#'   for `ard_stack_hierarchical_count()`.
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
#'   should be calculated (i.e. repeat the operations with `by=NULL` in _most cases_, see below for details).
#'   Default is `FALSE`.
#' @param over_variables (scalar `logical`)\cr
#' logical indicating whether summary statistics
#'   should be calculated over or across the columns listed in the `variables` argument.
#'   Default is `FALSE`.
#' @param attributes (scalar `logical`)\cr
#'   logical indicating whether to include the results of `ard_attributes()` for all
#'   variables represented in the ARD. Default is `FALSE`.
#' @param total_n (scalar `logical`)\cr
#'   logical indicating whether to include of `ard_total_n(denominator)` in the returned ARD.
#' @param shuffle (scalar `logical`)\cr
#'   logical indicating whether to perform `shuffle_ard()` on the final result.
#'   Default is `FALSE`.
#' @param sort (`string` or `NULL`)\cr
#'   Specifies sorting to perform. Values must be one of:
#'   - `"alphanumeric"` - within each hierarchical section of the ARD, rows are ordered alphanumerically (i.e. A to Z)
#'     by `variable_label` text.
#'   - `"descending"` - within each hierarchical section of the ARD, frequency sums are calculated for each row and rows
#'     are sorted in descending order by sum.
#'   Default is `NULL` (no sorting performed).
#'
#' @return an ARD data frame of class 'card'
#' @name ard_stack_hierarchical
#'
#' @examples
#' ard_stack_hierarchical(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   id = USUBJID
#' )
#'
#' ard_stack_hierarchical_count(
#'   ADAE,
#'   variables = c(AESOC, AEDECOD),
#'   by = TRTA,
#'   denominator = ADSL |> dplyr::rename(TRTA = ARM),
#'   sort = "alphanumeric"
#' )
NULL

#' @rdname ard_stack_hierarchical
#' @export
ard_stack_hierarchical <- function(data,
                                   variables,
                                   by = dplyr::group_vars(data),
                                   id,
                                   denominator,
                                   include = everything(),
                                   statistic = everything() ~ c("n", "N", "p"),
                                   overall = FALSE,
                                   over_variables = FALSE,
                                   attributes = FALSE,
                                   total_n = FALSE,
                                   shuffle = FALSE,
                                   sort = NULL) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(variables)
  check_not_missing(id)
  check_not_missing(denominator)
  cards::process_selectors(data, id = {{ id }})

  # denominator must a data frame, or integer
  if (!is.data.frame(denominator) && !is_integerish(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} argument must be a {.cls data.frame} or an {.cls integer}, not {.obj_type_friendly {denominator}}.",
      call = get_cli_abort_call()
    )
  }

  # check the id argument is not empty
  if (is_empty(id)) {
    cli::cli_abort("Argument {.arg id} cannot be empty.", call = get_cli_abort_call())
  }

  # create ARD -----------------------------------------------------------------
  internal_stack_hierarchical(
    data = data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }},
    denominator = denominator,
    include = {{ include }},
    statistic = statistic,
    overall = overall,
    over_variables = over_variables,
    attributes = attributes,
    total_n = total_n,
    shuffle = shuffle,
    sort = sort
  )
}

#' @rdname ard_stack_hierarchical
#' @export
ard_stack_hierarchical_count <- function(data,
                                         variables,
                                         by = dplyr::group_vars(data),
                                         denominator = NULL,
                                         include = everything(),
                                         overall = FALSE,
                                         over_variables = FALSE,
                                         attributes = FALSE,
                                         total_n = FALSE,
                                         shuffle = FALSE,
                                         sort = NULL) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(variables)
  # denominator must be empty, a data frame, or integer
  if (!is_empty(denominator) && !is.data.frame(denominator) && !is_integerish(denominator)) {
    cli::cli_abort(
      "The {.arg denominator} argument must be empty, a {.cls data.frame}, or an {.cls integer}, not {.obj_type_friendly {denominator}}.",
      call = get_cli_abort_call()
    )
  }

  # create ARD -----------------------------------------------------------------
  internal_stack_hierarchical(
    data = data,
    variables = {{ variables }},
    by = {{ by }},
    id = NULL,
    denominator = denominator,
    include = {{ include }},
    statistic = NULL,
    overall = overall,
    over_variables = over_variables,
    attributes = attributes,
    total_n = total_n,
    shuffle = shuffle,
    sort = sort
  )
}



internal_stack_hierarchical <- function(data,
                                        variables,
                                        by = dplyr::group_vars(data),
                                        id = NULL,
                                        denominator = NULL,
                                        include = everything(),
                                        statistic = NULL,
                                        overall = FALSE,
                                        over_variables = FALSE,
                                        attributes = FALSE,
                                        total_n = FALSE,
                                        shuffle = FALSE,
                                        include_uni_by_tab = TRUE,
                                        sort = NULL) {
  # process inputs -------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variables)
  cards::process_selectors(data, variables = {{ variables }}, id = {{ id }}, by = {{ by }})
  cards::process_selectors(data[variables], include = {{ include }})
  check_scalar_logical(overall)
  check_scalar_logical(over_variables)
  check_scalar_logical(attributes)
  check_scalar_logical(total_n)
  check_scalar_logical(shuffle)

  # check inputs ---------------------------------------------------------------
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
      "The last column specified in the {.arg variables} (i.e. {.val {utils::tail(variables, 1L)}}) must be in the {.arg include} argument.",
      call = get_cli_abort_call()
    )
  }

  if (is_empty(by) && isTRUE(overall)) {
    cli::cli_inform(
      c("The {.arg by} argument must be specified when using {.code overall=TRUE}.",
        i = "Setting {.code overall=FALSE}."
      )
    )
    overall <- FALSE
  }

  if (!is.data.frame(denominator) && isTRUE(overall)) {
    cli::cli_inform(
      c("The {.arg denominator} argument must be specified as a data frame when using {.code overall=TRUE}.",
        i = "Setting {.code overall=FALSE}."
      )
    )
    overall <- FALSE
  }

  if (is_empty(denominator) && isTRUE(total_n)) {
    cli::cli_inform(
      c("The {.arg denominator} argument must be specified when using {.code total_n=TRUE}.",
        i = "Setting {.code total_n=FALSE}."
      )
    )
    total_n <- FALSE
  }

  if (!sort %in% c("descending", "alphanumeric")) {
    cli::cli_abort(
      "The {.arg sort} argument must be either {.val descending} or {.val alphanumeric}.",
      call = get_cli_abort_call()
    )
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
                            {.val {NA}} or {.val {NaN}} values in {.val {intersect(by, names(denominator))}} column{?s}."))
      denominator <- denominator[!rows_with_na_denom, ]
    }
  }

  # sort data if using `ard_hierarchical(id)` ----------------------------------
  if (!is_empty(id)) data <- dplyr::arrange(data, dplyr::pick(all_of(c(id, by, variables)))) # styler: off

  # print denom columns if not 100% clear which are used
  if (!is_empty(id) && is.data.frame(denominator)) {
    denom_cols <- intersect(by, names(denominator))
    if (!setequal(by, denom_cols)) {
      msg <-
        ifelse(
          is_empty(denom_cols),
          "Denominator set by number of rows in {.arg denominator} data frame.",
          "Denominator set by {.val {denom_cols}} column{?s} in {.arg denominator} data frame."
        )
      cli::cli_inform(c("i" = msg))
    }
  }

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
          id = id,
          statistic = statistic
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
            id = id,
            statistic = statistic
          ) |>
            list()
        )

      # if there are columns in `by` not present in `denominator`, re-run with `by = NULL`
      if (!is_empty(setdiff(by, names(denominator)))) {
        lst_results <-
          lst_results |>
          append(
            .run_hierarchical_fun(
              data = data,
              variables = variables[seq_len(i)],
              by = NULL,
              denominator = denominator,
              id = id,
              statistic = statistic
            ) |>
              list()
          )
      }
    }
  }

  # add univariate tabulations of by variables ---------------------------------
  if (isTRUE(include_uni_by_tab) && is.data.frame(denominator) && !is_empty(intersect(by, names(denominator)))) {
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
  if (isTRUE(over_variables)) {
    lst_results <-
      lst_results |>
      append(
        # need to use this call to also re-run for `overall=TRUE` when specified
        rlang::call2(
          "internal_stack_hierarchical",
          data = expr(data |> dplyr::mutate(..ard_hierarchical_overall.. = TRUE)),
          variables = "..ard_hierarchical_overall..",
          by = by,
          id = id,
          include = "..ard_hierarchical_overall..",
          denominator = expr(denominator),
          statistic = statistic,
          overall = overall,
          over_variables = FALSE,
          attributes = FALSE,
          total_n = FALSE,
          shuffle = FALSE,
          include_uni_by_tab = FALSE
        ) %>%
          {suppressMessages(eval_tidy(.))} |> # styler: off
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
  } else if (isTRUE(total_n) && is_integerish(denominator)) {
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

  if (!is.null(sort)) {
    browser()

    overall <- "..ard_hierarchical_overall.." %in% result$variable
    outer_cols <- sapply(
      result |> dplyr::select(cards::all_ard_groups("names")),
      function(x) dplyr::last(unique(stats::na.omit(x)))
    )
    inner_col <- setdiff(
      result$variable,
      result |> dplyr::select(cards::all_ard_groups("names")) |> unlist() |> unique()
    )

    # keep summary rows at the top of each sub-section
    rep_str <- " "

    if (sort == "alphanumeric") {
      # sort by label -------------------------------------------------------------------------------
      sort_cols <- c(result |> dplyr::select(cards::all_ard_groups("levels")) |> names(), "inner_var", "label")

      result <- result |>
        dplyr::rowwise() |>
        dplyr::mutate(inner_var = if (!.data$variable %in% inner_col) rep_str else .data$variable) |>
        dplyr::ungroup() |>
        dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ tidyr::replace_na(., rep_str))) |>
        dplyr::arrange(across(all_of(sort_cols), ~ .x)) |>
        dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ str_replace(., paste0("^", rep_str, "$"), NA))) |>
        dplyr::select(-"inner_var")
    } else {
      # get row sums --------------------------------------------------------------------------------
      result <- .append_hierarchy_row_sums(result, .stat = "n")

      # append outer hierarchy level sums in each row to sort at all levels -------------------------
      for (g in names(outer_cols)) {
        result <- result |> dplyr::group_by(across(all_of(c(g, paste0(g, "_level")))), .add = TRUE)
        result <- result |>
          dplyr::left_join(
            result |>
              dplyr::summarize(!!paste0("sum_", g) := dplyr::first(.data$sum_row)),
            by = result |> dplyr::group_vars()
          )
      }

      # summary rows remain at the top of each sub-section
      result <- result |>
        dplyr::ungroup() |>
        dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ tidyr::replace_na(., rep_str))) |>
        dplyr::rowwise() |>
        dplyr::mutate(inner_var = if (!.data$variable %in% inner_col) rep_str else .data$variable) |>
        dplyr::ungroup()

      # sort by row sum -----------------------------------------------------------------------------
      sort_cols <- c(rbind(
        result |> dplyr::select(cards::all_ard_groups("names")) |> names(),
        result |> dplyr::select(starts_with("sum_group")) |> names(),
        result |> dplyr::select(cards::all_ard_groups("levels")) |> names()
      ), "inner_var", "sum_row", "label")

      result <- result |>
        dplyr::arrange(across(all_of(sort_cols), ~ if (is.numeric(.x)) dplyr::desc(.x) else .x)) |>
        dplyr::mutate(across(cards::all_ard_groups(), .fns = ~ str_replace(., "^ $", NA))) |>
        dplyr::select(-starts_with("sum_"), -"inner_var")
    }
  }

  # return final result --------------------------------------------------------
  result
}

# this function calculates either the counts or the rates of the events
.run_hierarchical_fun <- function(data, variables, by, denominator, id, statistic) {
  if (is_empty(id)) {
    ard_hierarchical_count(
      data = data,
      variables = all_of(variables),
      by = all_of(by)
    )
  } else {
    ard_hierarchical(
      data = data |>
        dplyr::slice_tail(n = 1L, by = all_of(c(id, intersect(by, names(denominator)), variables))),
      variables = all_of(variables),
      by = all_of(by),
      denominator = denominator,
      id = all_of(id),
      statistic = statistic
    )
  }
}

.append_hierarchy_row_sums <- function(x, .stat) {
  cards <- x

  if (!.stat %in% cards$stat_name) {
    cli::cli_abort(
      "The {.arg .stat} argument is {.val {(.stat)}} but this statistic is not present in {.arg x}. For all valid
      statistic options see the {.val stat_name} column of {.code x}.",
      call = get_cli_abort_call()
    )
  }

  by_cols <- if (ncol(x |> dplyr::select(starts_with("stat_"))) > 1) c("group1", "group1_level") else NA
  outer_cols <- sapply(
    x |> dplyr::select(cards::all_ard_groups("names")),
    function(x) dplyr::last(unique(stats::na.omit(x)))
  )

  # update logical variable_level entries from overall row to character
  cards$variable_level[cards$variable == "..ard_hierarchical_overall.."] <- "OVERALL" |> as.list()
  browser()

  # extract row sums ------------------------------------------------------------------------------
  cards <- cards |>
    dplyr::filter(.data$stat_name == .stat, .data$variable %in% x$variable) |>
    dplyr::group_by(across(c(cards::all_ard_groups(), cards::all_ard_variables(), -all_of(by_cols)))) |>
    dplyr::summarise(sum_row = sum(unlist(.data$stat))) |>
    dplyr::ungroup() |>
    dplyr::rename(label = "variable_level") |>
    tidyr::unnest(cols = everything())

  # match cards names to x -------------------------------------------------------------
  if (length(by_cols) > 2) {
    names(cards)[grep("group", names(cards))] <- x |>
      dplyr::select(cards::all_ard_groups()) |>
      names()
  }
  cards[cards$variable == "..ard_hierarchical_overall..", 1] <- "..ard_hierarchical_overall.."

  # fill in NAs to align cards layout with x -------------------------------------------
  cards <- cards |>
    dplyr::rowwise() |>
    dplyr::mutate(across(
      cards::all_ard_groups(),
      ~ if (is.na(.x) && !grepl("_level", dplyr::cur_column()) && .data$variable == outer_cols[dplyr::cur_column()]) {
        .data$variable
      } else if (is.na(.x) && .data$variable %in% outer_cols[gsub("_level", "", dplyr::cur_column())]) {
        .data$label
      } else {
        .x
      }
    ))

  # for any variables not in include, calculate group sums ----------------------------------------
  if (!all(outer_cols %in% cards$variable)) {
    gp_vars <- outer_cols[outer_cols %in% setdiff(outer_cols, cards$variable)]
    gp_cols <- names(gp_vars)

    cli::cli_inform(
      "Not all hierarchy variables present in the ARD were included in the {.arg include} argument.
      These variables ({gp_vars}) do not have event rate data available so the total sum of the event
      rates for this hierarchy section will be used instead. To use event rates for all sections of the ARD,
      set {.code include = everything()} when creating your ARD via {.fun ard_stack_hierarchical}."
    )

    for (i in seq_along(gp_cols)) {
      cards <- cards |>
        dplyr::bind_rows(
          cards |>
            dplyr::filter(.data$variable != "..ard_hierarchical_overall..") |>
            dplyr::group_by(across(c(gp_cols[1:i], paste0(gp_cols[1:i], "_level")))) |>
            dplyr::summarize(sum_row = sum(.data$sum_row)) |>
            dplyr::mutate(
              variable = .data[[gp_cols[i]]],
              label = .data[[paste0(gp_cols[i], "_level")]]
            )
        )
    }
  }

  # append row sums to x ---------------------------------------------------------------
  x <- x |>
    dplyr::left_join(
      cards,
      by = c(cards |> dplyr::select(-"sum_row") |> names())
    )

  x
}

