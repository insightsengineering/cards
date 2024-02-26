#' Shuffle ARD
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
  check_class(x = x, cls = "card")
  check_scalar_logical(trim)

  # make sure columns are in order & add index for retaining order
  dat_cards <- x |>
    tidy_ard_column_order() |>
    tidy_ard_row_order() |>
    dplyr::mutate(.cards_idx = dplyr::row_number())

  # fill stat label if missing
  dat_cards <- dat_cards |>
    dplyr::mutate(dplyr::across(any_of("stat_label"), ~ dplyr::coalesce(.x, stat_name)))

  # split up the data into data/variable info & cards info
  vars_ard <- dat_cards |>
    dplyr::select(all_ard_groups(), all_ard_variables()) |>
    names()
  vars_protected <- setdiff(names(dat_cards), vars_ard)

  dat_cards_grps <- dat_cards |>
    dplyr::select(-all_of(vars_protected), ".cards_idx")

  dat_cards_stats <- dat_cards |>
    dplyr::select(all_of(vars_protected))

  # process the data/variable info
  dat_cards_grps_processed <- dat_cards_grps |>
    # unlist the list-columns
    dplyr::mutate(
      dplyr::across(
        where(.is_list_column_of_scalars),
        ~ lapply(., \(x) if (!is.null(x)) x else NA_character_) |> unlist()
      )
    ) |>
    .check_var_nms(vars_protected = names(dat_cards_stats)) |>
    .rnm_grp_vars() |>
    .fill_grps_from_variables()

  # join together again
  dat_cards_out <- dplyr::left_join(
    dat_cards_grps_processed,
    dat_cards_stats,
    by = ".cards_idx"
  )

  # fill stat_label -> variable_level if exists
  if ("stat_label" %in% names(dat_cards_out)) {
    dat_cards_out <- dat_cards_out |>
      dplyr::mutate(dplyr::across(any_of("variable_level"), ~ dplyr::coalesce(.x, stat_label)))
  }

  dat_cards_out <- dat_cards_out |>
    dplyr::rename(any_of(c(label = "variable_level"))) |>
    dplyr::arrange(".cards_idx") |>
    dplyr::select(-".cards_idx")

  if (trim) {
    dat_cards_out |> .trim_ard()
  } else {
    dat_cards_out
  }
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
#' @noRd
.trim_ard <- function(x) {
  check_data_frame(x)

  # flatten ard table for easier viewing ---------------------------------------
  x |>
    # detect any warning/error messages and notify user
    .detect_msgs("warning", "error") |>
    # filter to numeric statistic values
    dplyr::filter(map_lgl(
      .data$stat,
      \(x) is.null(x) || (length(x) == 1L && (is.numeric(x) || is.na(x)))
    )) |>
    # unlist the list-columns
    dplyr::mutate(stat = lapply(
      .data$stat,
      \(x) if (!is.null(x) && !is.na(x)) x else NA_real_
    ) |> unlist() |> unname()) |>
    # remove the formatting functions / warning / error
    dplyr::select(-where(is.list), -any_of("stat_label"))
}


#' Detect Columns with Non-Null Contents
#'
#' Function looks for non-null contents in requested columns and notifies user
#' before removal. Specifically used for detecting messages.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#' @param ... ([`dynamic-dots`][dyn-dots])\cr
#'   columns to search within
#'
#' @return a data frame
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
#' @noRd
.detect_msgs <- function(x, ...) {
  dots <- rlang::dots_list(...)

  lapply(dots, function(var) {
    if (any(!map_lgl(x[[var]], is.null))) {
      cli::cli_inform("{.val {var}} column contains messages that will be removed.")
    }
  })

  x
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
#' data <- data.frame(a = "x", b = "y", c = "z", .cards_idx = 1)
#'
#' cards:::.check_var_nms(data, vars_protected = c("x", "z"))
#' @noRd
.check_var_nms <- function(x, vars_protected) {
  # get all represented variable names from original data
  var_nms <- x |>
    dplyr::select(-ends_with("_level"), -".cards_idx") |>
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
        -c(ends_with("_level"), ".cards_idx"),
        ~ dplyr::recode(.x, !!!var_nms_new)
      ))
  } else {
    x
  }
}


#' Rename Group Variables
#'
#' This function combines each pair of `group` and `group_level` columns into a
#' single column. The `group_level` column is renamed according to the value of
#' the `group` column.
#'
#' @param x (`data.frame`)\cr
#'   a data frame
#'
#' @return data frame
#' @keywords internal
#'
#' @examples
#' data <- data.frame(group1 = "A", x = "B", group2 = "C", y = "D")
#'
#' cards:::.rnm_grp_vars(data)
#' @noRd
.rnm_grp_vars <- function(x) {
  grp_var_levs <- names(x)[grep("^group[0-9]+_level$", names(x))]
  grp_vars <- names(x)[grep("^group[0-9]+$", names(x))]

  if (length(grp_vars) == 0) {
    return(x)
  }

  # loop through each of the grouping variables
  for (v in grp_vars) {
    # rename as the variable level within the unique levels of the grouping variable
    x <- x |>
      dplyr::mutate(!!v := fct_inorder(.data[[v]])) |>
      dplyr::group_by(.data[[v]]) |>
      dplyr::group_split() |>
      map(function(dat) {
        v_lev <- paste0(v, "_level")
        v_new <- unique(dat[[v]]) |> as.character()

        # drop if no grouping values
        if (is.na(v_new)) {
          dplyr::select(dat, -any_of(c(v_lev, v)))
        } else {
          # create _level var if it does not exist
          if (is.null(dat[[v_lev]])) {
            dat <- dat |> dplyr::mutate(!!v_lev := NA_character_)
          }

          # fill any NA _level
          v_new_fill <- make.unique(c(
            unique(dat[[v_lev]]),
            paste("Overall", v_new)
          )) |>
            dplyr::last()

          # rename _level var & drop source
          dat %>%
            dplyr::mutate(!!v_lev := tidyr::replace_na(.data[[v_lev]], v_new_fill)) |>
            dplyr::rename(!!v_new := all_of(v_lev)) |>
            dplyr::select(-all_of(v))
        }
      }) |>
      dplyr::bind_rows()
  }

  x |>
    dplyr::relocate(all_ard_variables(), any_of(".cards_idx"), .after = last_col())
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
#' @noRd
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
    dplyr::bind_rows()
}

#' List Column as a Vector Predicate
#'
#' A predicate function to check whether a column is a list and can be
#' represented as a vector.
#'
#' @param x (`any`)\cr
#'   column to check
#'
#' @return a logical
#' @keywords internal
#'
#' @examples
#' cards:::.is_list_column_of_scalars(as.list(1:5))
#' @noRd
.is_list_column_of_scalars <- function(x) {
  is.list(x) && all(unlist(lapply(x, FUN = function(x) length(x) == 1L || is.null(x))))
}
