#' Comparison ARD Statistics
#'
#' @description
#' ARD functions for comparing values between groups.
#'
#' `ard_ttest()` -> `t.test(data[[variable]] ~ data[[by]], ...)`
#' `ard_wilcoxtest()` -> `wilcox.test(data[[variable]] ~ data[[by]], ...)`
#' `ard_chisqtest()` -> `chisq.test(x = data[[variable]], y = data[[by]], ...)`
#' `ard_fishertest()` -> `fisher.test(x = data[[variable]], y = data[[by]], ...)`
#'
#' @inheritParams ard_continuous
#' @param by character column name to compare by
#' @param variable character column name to be compared
#' @param ... arguments passed to method.
#'
#' @return data frame
#' @name ard_comparison
#'
#' @examples
#' ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_ttest(by = "ARM", variable = "AGE") |>
#'   flatten_ard()
#'
#' ADSL |>
#'   dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
#'   ard_wilcoxtest(by = "ARM", variable = "AGE") |>
#'   flatten_ard()
#'
#' ADSL |>
#'   ard_chisqtest(by = "ARM", variable = "AGEGR1") |>
#'   flatten_ard()
#'
#' ADSL[1:30,] |>
#'   ard_fishertest(by = "ARM", variable = "AGEGR1") |>
#'   flatten_ard()
NULL

#' @rdname ard_comparison
#' @export
ard_ttest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  data <- dplyr::ungroup(data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # perform t-test and format results ------------------------------------------
  lst_ttest <- eval_capture_conditions(stats::t.test(data[[variable]] ~ data[[by]], ...))

  # if there are results, put them in the ARD format ---------------------------
  if (!is.null(lst_ttest[["result"]])) {
    # additional args passed by user (and default values) will appended to broom::tidy() results
    df_ttest_args <-
      utils::modifyList(
        # grab the default arg values
        x = formals(asNamespace("stats")[["t.test.default"]])[c("mu", "paired", "var.equal", "conf.level")],
        # update with any values passed by the user
        val = rlang::dots_list(...)
      ) |>
      dplyr::as_tibble()

    # tidy results, then put them in ARD format
    ret <-
      lst_ttest[["result"]] |>
      broom::tidy() |>
      dplyr::bind_cols(df_ttest_args) |>
      dplyr::mutate(
        dplyr::across(everything(), .fns = list),
        group1 = .env$by,
        variable = .env$variable
      ) |>
      tidyr::pivot_longer(
        cols = -c("group1", "variable"),
        names_to = "stat_name",
        values_to = "statistic"
      ) |>
      dplyr::mutate(
        group1_level =
          dplyr::case_when(
            .data$stat_name %in% "estimate1" ~ unique(data[[by]]) |> stats::na.omit() |>  sort() |> dplyr::first() |> list(),
            .data$stat_name %in% "estimate2" ~ unique(data[[by]]) |> stats::na.omit() |>sort() |> dplyr::last() |> list(),
          )
      )
  }

  # if there was an error, return empty data frame in ARD format ---------------
  else {
    ret <-
      dplyr::tibble(
        group1 = .env$by,
        variable = .env$variable,
        stat_name = NA_character_,
        statistic = list(NULL)
      )
  }

  # return and add warning/errors ----------------------------------------------
  ret |>
    dplyr::mutate(
      context = "ttest",
      warning = lst_ttest["warning"],
      error = lst_ttest["error"]
    ) %>%
    structure(., class = c("card", class(.)))
}

#' @rdname ard_comparison
#' @export
ard_wilcoxtest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # perform Wilcoxon test and format results -----------------------------------
  lst_wilcox <- eval_capture_conditions(stats::wilcox.test(data[[variable]] ~ data[[by]], ...))

  # if there are results, put them in the ARD format ---------------------------
  if (!is.null(lst_wilcox[["result"]])) {
    # additional args passed by user (and default values) will appended to broom::tidy() results
    df_wilcoxtest_args <-
      utils::modifyList(
        # grab the default arg values
        x = formals(asNamespace("stats")[["wilcox.test.default"]]) %>%
          `[`(c("mu", "paired", "exact", "correct", "conf.int",
                "conf.level", "tol.root", "digits.rank")),
        # update with any values passed by the user
        val = rlang::dots_list(...),
        keep.null = FALSE
      ) |>
      lapply(function(x) list(x)) |>
      dplyr::as_tibble()

    ret <-
      broom::tidy(lst_wilcox[["result"]]) |>
      dplyr::mutate(
        dplyr::across(everything(), .fns = list),
        group1 = .env$by,
        variable = .env$variable
      ) |>
      dplyr::bind_cols(df_wilcoxtest_args) |>
      tidyr::pivot_longer(
        cols = -c("group1", "variable"),
        names_to = "stat_name",
        values_to = "statistic"
      )
  }

  # if there was an error, return empty data frame in ARD format ---------------
  else {
    ret <-
      dplyr::tibble(
        group1 = .env$by,
        variable = .env$variable,
        stat_name = NA_character_,
        statistic = list(NULL)
      )
  }

  # return and add warning/errors ----------------------------------------------
  ret |>
    dplyr::mutate(
      context = "wilcoxtest",
      warning = lst_wilcox["warning"],
      error = lst_wilcox["error"]
    ) %>%
    structure(., class = c("card", class(.)))
}


#' @rdname ard_comparison
#' @export
ard_chisqtest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # perform chisq test and format results -----------------------------------
  lst_chisq <-
    eval_capture_conditions(stats::chisq.test(x = data[[variable]], y = data[[by]], ...))

  # if there are results, put them in the ARD format ---------------------------
  if (!is.null(lst_chisq[["result"]])) {
    # additional args passed by user (and default values) will appended to broom::tidy() results
    df_chisqtest_args <-
      utils::modifyList(
        # grab the default arg values
        x = formals(stats::chisq.test) %>%
          `[`(c("correct", "p", "rescale.p", "simulate.p.value", "B")),
        # update with any values passed by the user
        val = rlang::dots_list(...),
        keep.null = FALSE
      ) |>
      lapply(function(x) list(x)) |>
      dplyr::as_tibble()

    ret <-
      broom::tidy(lst_chisq[["result"]]) |>
      dplyr::mutate(
        dplyr::across(everything(), .fns = list),
        group1 = .env$by,
        variable = .env$variable
      ) |>
      dplyr::bind_cols(df_chisqtest_args) |>
      tidyr::pivot_longer(
        cols = -c("group1", "variable"),
        names_to = "stat_name",
        values_to = "statistic"
      )
  }

  # if there was an error, return empty data frame in ARD format ---------------
  else {
    ret <-
      dplyr::tibble(
        group1 = .env$by,
        variable = .env$variable,
        stat_name = NA_character_,
        statistic = list(NULL)
      )
  }

  # return and add warning/errors ----------------------------------------------
  ret |>
    dplyr::mutate(
      context = "chisqtest",
      warning = lst_chisq["warning"],
      error = lst_chisq["error"]
    ) %>%
    structure(., class = c("card", class(.)))
}

#' @rdname ard_comparison
#' @export
ard_fishertest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom")

  # check/process inputs -------------------------------------------------------
  check_not_missing(data, "data")
  check_not_missing(variable, "variable")
  check_not_missing(by, "by")
  check_class_data_frame(data = data)
  process_selectors(data, by = {{ by }}, variable = {{ variable }})
  check_length(by, "by", 1L)
  check_length(variable, "variable", 1L)

  # perform fisher test and format results -----------------------------------
  lst_tidy_fisher <-
    eval_capture_conditions(
      stats::fisher.test(x = data[[variable]], y = data[[by]], ...) |>
        broom::tidy()
    )

  # build ARD ------------------------------------------------------------------
  tidy_as_ard(
    lst_tidy = lst_tidy_fisher,
    tidy_result_names =
      c("estimate", "p.value", "conf.low", "conf.high", "method", "alternative"),
    fun_args_to_record =
      c("workspace", "hybrid", "hybridPars", "control", "or",
        "conf.int", "conf.level", "simulate.p.value", "B"),
    formals = formals(stats::fisher.test),
    passed_args = rlang::dots_list(...),
    context = "fishertest",
    lst_ard_columns = list(group1 = by, variable = variable)
  )
}


