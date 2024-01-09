#' Comparison ARD Statistics
#'
#' @description
#' ARD functions for comparing values between groups.
#'
#' `ard_ttest()` -> `t.test(data[[variable]] ~ data[[by]], ...)`
#'
#' `ard_wilcoxtest()` -> `wilcox.test(data[[variable]] ~ data[[by]], ...)`
#'
#' `ard_chisqtest()` -> `chisq.test(x = data[[variable]], y = data[[by]], ...)`
#'
#' `ard_fishertest()` -> `fisher.test(x = data[[variable]], y = data[[by]], ...)`
#'
#' @inheritParams ard_continuous
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   character column name to compare by
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   character column name to be compared
#' @param ... arguments passed to method.
#'
#' @return data frame
#' @name ard_comparison
#'
#' @examples
#' ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_ttest(by = "ARM", variable = "AGE")
#'
#' ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_wilcoxtest(by = "ARM", variable = "AGE")
#'
#' ADSL |>
#'   ard_chisqtest(by = "ARM", variable = "AGEGR1")
#'
#' ADSL[1:30,] |>
#'   ard_fishertest(by = "ARM", variable = "AGEGR1")
NULL

#' @rdname ard_comparison
#' @export
ard_ttest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom.helpers", pkg_search = "cards")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  data <- dplyr::ungroup(data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # build ARD ------------------------------------------------------------------
  ret <-
    tidy_as_ard(
      lst_tidy =
        eval_capture_conditions(
          stats::t.test(data[[variable]] ~ data[[by]], ...) |>
            broom::tidy()
        ),
      tidy_result_names = c("estimate", "estimate1", "estimate2", "statistic",
                            "p.value", "parameter", "conf.low", "conf.high",
                            "method", "alternative"),
      fun_args_to_record = c("mu", "paired", "var.equal", "conf.level"),
      formals = formals(asNamespace("stats")[["t.test.default"]]),
      passed_args = dots_list(...),
      lst_ard_columns = list(group1 = by, variable = variable, context = "ttest")
    )

  # add the estimate levels and return object ----------------------------------
  tryCatch({
    group1_levels <-
      unique(data[[by]]) |> stats::na.omit() |>  sort()
    if (length(group1_levels) != 2L) stop("generic message that no one will see.")

    ret |>
      dplyr::mutate(
        group1_level =
          dplyr::case_when(
            .data$stat_name %in% "estimate1" ~ dplyr::first(group1_levels) |> list(),
            .data$stat_name %in% "estimate2" ~ dplyr::last(group1_levels) |> list(),
          )
       )},
    # in case of an error, simply return ARD without these levels
    error = function(e) dplyr::mutate(ret, group1_level = list(NULL))
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "estimate1" ~ "Group 1 Mean",
          .data$stat_name %in% "estimate2" ~ "Group 2 Mean",
          .data$stat_name %in% "estimate" ~ "Mean Difference",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "statistic" ~ "t Statistic",
          .data$stat_name %in% "parameter" ~ "Degrees of Freedom",
          .data$stat_name %in% "conf.low" ~ "CI Lower Bound",
          .data$stat_name %in% "conf.high" ~ "CI Upper Bound",
          .data$stat_name %in% "mu" ~ "H0 Mean",
          .data$stat_name %in% "paired" ~ "Paired t-test",
          .data$stat_name %in% "var.equal" ~ "Equal Variances",
          .data$stat_name %in% "conf.level" ~ "CI Confidence Level",
          TRUE ~ .data$stat_name
        )
    ) |>
    tidy_ard_column_order()
}

#' @rdname ard_comparison
#' @export
ard_wilcoxtest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom.helpers", pkg_search = "cards")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # build ARD ------------------------------------------------------------------
  tidy_as_ard(
    lst_tidy =
      eval_capture_conditions(
        stats::wilcox.test(data[[variable]] ~ data[[by]], ...) |>
          broom::tidy()
      ),
    tidy_result_names = c("statistic", "p.value", "method", "alternative"),
    fun_args_to_record =
      c("mu", "paired", "exact", "correct", "conf.int",
        "conf.level", "tol.root", "digits.rank"),
    formals = formals(asNamespace("stats")[["wilcox.test.default"]]),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "wilcoxtest")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "W Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "paired" ~ "Paired test",
          .data$stat_name %in% "correct" ~ "Continuity Correction",
          TRUE ~ .data$stat_name
        )
    )
}


#' @rdname ard_comparison
#' @export
ard_chisqtest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom.helpers", pkg_search = "cards")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # build ARD ------------------------------------------------------------------
  tidy_as_ard(
    lst_tidy =
      eval_capture_conditions(
        stats::chisq.test(x = data[[variable]], y = data[[by]], ...) |>
          broom::tidy()
      ),
    tidy_result_names = c("statistic", "p.value", "parameter", "method"),
    fun_args_to_record =
      c("correct", "p", "rescale.p", "simulate.p.value", "B"),
    formals = formals(stats::chisq.test),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "chisqtest")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "statistic" ~ "X-squared Statistic",
          .data$stat_name %in% "p.value" ~ "p-value",
          .data$stat_name %in% "paramter" ~ "Degrees of Freedom",
          TRUE ~ .data$stat_name,
        )
    )
}

#' @rdname ard_comparison
#' @export
ard_fishertest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  check_pkg_installed("broom.helpers", pkg_search = "cards")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # build ARD ------------------------------------------------------------------
  tidy_as_ard(
    lst_tidy =
      eval_capture_conditions(
        stats::fisher.test(x = data[[variable]], y = data[[by]], ...) |>
          broom::tidy()
      ),
    tidy_result_names =
      c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
    fun_args_to_record =
      c("workspace", "hybrid", "hybridPars", "control", "or",
        "conf.int", "conf.level", "simulate.p.value", "B"),
    formals = formals(stats::fisher.test),
    passed_args = dots_list(...),
    lst_ard_columns = list(group1 = by, variable = variable, context = "fishertest")
  ) |>
    dplyr::mutate(
      .after = "stat_name",
      stat_label =
        dplyr::case_when(
          .data$stat_name %in% "p.value" ~ "p-value",
          TRUE ~ .data$stat_name,
        )
    )
}


