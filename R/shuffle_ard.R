#' Shuffle ARD
#'
#' @description `r lifecycle::badge('experimental')`\cr
#'
#' This function ingests an ARD object and shuffles the information to prepare for analysis.
#' Helpful for streamlining across multiple ARDs. Combines each group/group_level into 1
#' column, back fills missing grouping values from the variable levels where possible, and
#' optionally trims statistics-level metadata.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param trim (`logical`)\cr
#'   logical representing whether or not to trim away statistic-level metadata and filter
#'   only on numeric statistic values.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' bind_ard(
#'   ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
#'   ard_categorical(ADSL, variables = "ARM")
#' ) |>
#'   shuffle_ard()
shuffle_ard <- function(x, trim = TRUE) {
  set_cli_abort_call()

  check_class(x = x, cls = "card")
  check_scalar_logical(trim)


  ard_attributes <- attributes(x)
  ard_args <- ard_attributes$args

  # make sure columns are in order & add index for retaining order
  dat_cards <- x |>
    tidy_ard_column_order() |>
    tidy_ard_row_order() |>
    dplyr::mutate(..cards_idx.. = dplyr::row_number())

  # fill stat label if missing
  dat_cards <- dat_cards |>
    dplyr::mutate(dplyr::across(any_of("stat_label"), ~ dplyr::coalesce(.x, stat_name)))

  # split up the data into data/variable info & cards info
  vars_ard <- dat_cards |>
    dplyr::select(all_ard_groups(), all_ard_variables()) |>
    names()

  vars_protected <- setdiff(names(dat_cards), vars_ard)

  dat_cards_grps <- dat_cards |>
    dplyr::select(-all_of(vars_protected), "..cards_idx..")

  dat_cards_stats <- dat_cards |>
    dplyr::select(all_of(vars_protected))

  # process the data/variable info
  dat_cards_grps_processed <- dat_cards_grps |>
    .check_var_nms(vars_protected = names(dat_cards_stats)) |>
    rename_ard_columns(columns = all_ard_groups("names"), fill = "..cards_overall..") |>
    # coerce everything to character
    dplyr::mutate(
      dplyr::across(
        -"..cards_idx..",
        ~ lapply(., \(x) if (!is.null(x)) as.character(x) else NA_character_)
      )
    )

  # join together again
  dat_cards_out <- dplyr::left_join(
    dat_cards_grps_processed,
    dat_cards_stats,
    by = "..cards_idx.."
  )

  dat_cards_out <- dat_cards_out |>
    # unlist the list-columns
    unlist_ard_columns() |>
    .fill_grps_from_variables() |>
    .fill_overall_grp_values(vars_protected) |>
    dplyr::arrange(.data$..cards_idx..) |>
    dplyr::select(-"..cards_idx..")

  output <- dat_cards_out

  if (trim) {
    output <- dat_cards_out |>
      .trim_ard()
  }

  # re-attach the args attribute
  attr(output, "args") <- ard_args

  output
}


#' Trim ARD
#'
#' This function ingests an ARD object and trims columns and rows for downstream use in
#' displays. The resulting data frame contains only numeric results, no supplemental
#' information about errors/warnings, and unnested list columns.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return a tibble
#' @keywords internal
#'
#' @examples
#' ard <- bind_ard(
#'   ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
#'   ard_categorical(ADSL, variables = "ARM")
#' ) |>
#'   shuffle_ard(trim = FALSE)
#'
#' ard |> cards:::.trim_ard()
.trim_ard <- function(x) {
  check_data_frame(x)

  # detect any warning/error messages and notify user
  .detect_msgs(x, "warning", "error")
  # flatten ard table for easier viewing ---------------------------------------
  x |>
    dplyr::select(-c("fmt_fun", "warning", "error"))
}


#' Detect Columns with Non-Null Contents
#'
#' Function looks for non-null contents in requested columns and notifies user
#' before removal. Specifically used for detecting messages.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param ... ([`dynamic-dots`][rlang::dyn-dots])\cr
#'   columns to search within
#' @keywords internal
#'
#' @examples
#' ard <- ard_continuous(
#'   ADSL,
#'   by = ARM,
#'   variables = AGE,
#'   statistic = ~ list(
#'     mean = \(x) mean(x),
#'     mean_warning = \(x) {
#'       warning("warn1")
#'       warning("warn2")
#'       mean(x)
#'     },
#'     err_fn = \(x) stop("'tis an error")
#'   )
#' )
#'
#' cards:::.detect_msgs(ard, "warning", "error")
.detect_msgs <- function(x, ...) {
  dots <- rlang::dots_list(...)

  lapply(dots, function(var) {
    if (any(!map_lgl(x[[var]], is.null))) {
      cli::cli_inform("{.val {var}} column contains messages that will be removed.")
    }
  })
}

#' Check Variable Names
#'
#' Checks variable names in a data frame against protected names and modifies
#' them if needed
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param vars_protected (`character`)\cr
#'   a character vector of protected names
#'
#' @return a data frame
#' @keywords internal
#'
#' @examples
#' data <- data.frame(a = "x", b = "y", c = "z", ..cards_idx.. = 1)
#'
#' cards:::.check_var_nms(data, vars_protected = c("x", "z"))
.check_var_nms <- function(x, vars_protected) {
  # get all represented variable names from original data
  var_nms <- x |>
    dplyr::select(-ends_with("_level"), -"..cards_idx..") |>
    unlist(use.names = FALSE) |>
    unique()

  # create uniqueness across all variables from original data & cards-specific
  # variables
  var_nms_new <- make.unique(c(vars_protected, var_nms)) |>
    utils::tail(n = length(var_nms)) |>
    set_names(var_nms)

  # subset to only the ones needing recoding
  var_nms_new <- var_nms_new[imap(
    var_nms_new,
    function(x, y) {
      if (is.na(x)) FALSE else !x == y
    }
  ) |>
    unlist(use.names = FALSE)]

  # perform recodes if needed
  if (length(var_nms_new) > 0) {
    x |>
      dplyr::mutate(dplyr::across(
        -c(ends_with("_level"), "..cards_idx.."),
        ~ dplyr::recode(.x, !!!var_nms_new)
      ))
  } else {
    x
  }
}

#' Back Fill Group Variables
#'
#' This function back fills the values of group variables using
#' variable/variable_levels. The back filling will occur if the value of the
#' `variable` column matches the name of a grouping variable, and the grouping
#' variable's value is `NA`.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return data frame
#' @keywords internal
#'
#' @examples
#' data <- data.frame(
#'   variable = c(rep("A", 3), rep("B", 2)),
#'   variable_level = 1:5,
#'   A = rep(NA, 5),
#'   B = rep(NA, 5)
#' )
#'
#' cards:::.fill_grps_from_variables(data)
.fill_grps_from_variables <- function(x) {
  # within each variable, check if there is a match against one of the grouping cols
  # if the corresponding value in that grouping col is missing, backfill with the variable level
  x %>%
    dplyr::mutate(variable = fct_inorder(.data$variable)) |>
    dplyr::group_by(.data$variable) |>
    dplyr::group_split() |>
    map(function(dat) {
      grp_match <- names(dat)[names(dat) == unique(dat$variable)]
      if (length(grp_match) > 0 && "variable_level" %in% names(dat)) {
        dat |>
          dplyr::mutate(!!grp_match := ifelse(is.na(.data[[grp_match]]),
            .data$variable_level,
            .data[[grp_match]]
          ))
      } else {
        dat
      }
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(variable = as.character(.data$variable))
}


#' Fill Overall Group Variables
#'
#' This function fills the missing values of grouping variables with
#' `"Overall <variable_name>"` or `"Any <variable_name>"`where relevant.
#' Specifically, it will modify grouping values from rows with likely overall
#' calculations present (e.g. non-missing variable/variable_level, missing group
#' variables, and evidence that the `variable` has been computed by group in
#' other rows). `"Overall"` values will be populated only for grouping variables
#' that have been used in other calculations of the same variable and statistics.
#' `"Any"` will be used if it is likely to be a hierarchical calculation.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return data frame
#' @keywords internal
#'
#' @examples
#' data <- dplyr::tibble(
#'   grp = c("AA", "AA", NA, "BB", NA),
#'   variable = c("A", "B", "A", "C", "C"),
#'   variable_level = c(1, 2, 1, 3, 3),
#'   A = rep(NA, 5),
#'   B = rep(NA, 5),
#'   ..cards_idx.. = c(1:5)
#' )
#'
#' cards:::.fill_overall_grp_values(data, vars_protected = "..cards_idx..")
.fill_overall_grp_values <- function(x, vars_protected) {
  # determine grouping and merging variables
  id_vars <- c("variable", "variable_level", "stat_name", "stat_label")
  id_vars <- id_vars[id_vars %in% names(x)]
  grp_vars <- setdiff(names(x), unique(c(vars_protected, id_vars)))

  # replace NA group values with "..cards_overall.." where it is likely to be an overall calculation
  for (g in grp_vars) {
    # rows with missing group
    x_missing_by <- x |>
      dplyr::filter(is.na(.data[[g]]))

    # rows with non-missing group
    x_nonmissing_by <- x |>
      dplyr::filter(!is.na(.data[[g]]) & !.data[[g]] == "..cards_overall..")

    if (nrow(x_missing_by) > 0 && nrow(x_nonmissing_by) > 0) {
      x_missing_by_replaced <- x_missing_by |>
        dplyr::rows_update(
          x_nonmissing_by |>
            dplyr::mutate(!!g := ifelse(!is.na(.data[[g]]), "..cards_overall..", .data[[g]])) |>
            dplyr::select(-any_of(c(setdiff(names(x), c(g, id_vars))))) |>
            dplyr::distinct(),
          by = id_vars,
          unmatched = "ignore"
        )

      x <- dplyr::rows_update(x, x_missing_by_replaced, by = "..cards_idx..")
    }
  }

  # replace NA group values with "..cards_overall.." or "..hierarchical_overall.."
  # where it is likely to be a group or subgroup calculation
  for (i in seq_along(grp_vars)) {
    g_var <- grp_vars[i]

    x <- x |>
      dplyr::mutate(
        !!g_var := dplyr::case_when(
          # only assign "..cards_overall.." for the first grouping variable
          is.na(.data[[g_var]]) &
            .data$variable == "..ard_total_n.." & i == 1 ~
            "..cards_overall..",
          is.na(.data[[g_var]]) &
            .data$variable == "..ard_hierarchical_overall.." ~
            "..hierarchical_overall..",
          TRUE ~ .data[[g_var]]
        )
      )
  }

  # replace `"..cards_overall.."` group values with "Overall <colname>" and
  # `"..hierarchical_overall.."` with `"Any <colname>"`
  output <- x |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(
          grp_vars
        ),
        .derive_overall_labels
      )
    )

  output
}

#' Derive overall labels
#'
#' Transform the `"..cards_overall.."` and `"..hierarchical_overall.."` labels
#' into `"Overall <variable_name>"` and `"Any <variable_name>"` respectively.
#' Also it ensures the labels are unique (in case they already exist) with
#' `make.unique()` which appends a sequence number.
#'
#' @param x (character) content of target (current) column
#' @param cur_col (character) name of current column
#'
#' @returns a character vector
#'
#' @keywords internal
#'
#' @examples
#' data <- dplyr::tibble(
#'   ARM = c("..cards_overall..", "Overall ARM", NA, "BB", NA),
#'   TRTA = c(NA, NA, "..hierarchical_overall..", "C", "C")
#' )
#'
#' data |>
#'   dplyr::mutate(
#'     dplyr::across(
#'       ARM:TRTA,
#'       cards:::.derive_overall_labels
#'     )
#'   )
.derive_overall_labels <- function(x, cur_col = dplyr::cur_column()) {
  glue_overall <- glue::glue("Overall {cur_col}")
  glue_any <- glue::glue("Any {cur_col}")

  overall_val <- c(unique(x), glue_overall) |>
    make.unique() |>
    dplyr::last()
  any_val <- c(unique(x), glue_any) |>
    make.unique() |>
    dplyr::last()

  if (overall_val != glue_overall) {
    cli::cli_alert_info(
      "{.val {glue_overall}} already exists in the {.code {cur_col}} column. \\
      Using {.val {overall_val}}."
    )
  }

  if (any_val != glue_any) {
    cli::cli_alert_info(
      "{.val {glue_any}} already exists in the {.code {cur_col}} column. Using\\
       {.val {any_val}}."
    )
  }

  output <- dplyr::case_when(
    x == "..cards_overall.." ~ overall_val,
    x == "..hierarchical_overall.." ~ any_val,
    TRUE ~ x
  )

  output
}
