#' Comparison ARD Statistics
#'
#' @param data a data frame
#' @param by character column name to compare by
#' @param variable character column name to be compared
#' @param ... arguments passed to method.
#'
#' @return data frame
#' @name ard_comparison
#'
#' @examples
#' ard_ttest(data = ADSL, by = "ARM", variable = "AGE") |>
#'   flatten_ard()
#' ard_wilcoxtest(data = ADSL, by = "ARM", variable = "AGE") |>
#'   flatten_ard()
NULL

#' @rdname ard_comparison
#' @export
ard_ttest <- function(data, by, variable, ...) {
  # check installed packages ---------------------------------------------------
  rlang::check_installed("broom")

  # perform t-test and format results ------------------------------------------
  lst_ttest <- eval_capture_conditions(stats::t.test(data[[variable]] ~ data[[by]], ...))

  # if there are results, put them in the ARD format ---------------------------
  if (!is.null(lst_ttest[["result"]])) {
    ret <-
      lst_ttest[["result"]] |>
      broom::tidy() |>
      dplyr::mutate(
        conf.level = attr(lst_ttest[["result"]][["conf.int"]], "conf.level"),
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

  # perform Wilcoxon test and format results -----------------------------------
  lst_wilcox <- eval_capture_conditions(stats::wilcox.test(data[[variable]] ~ data[[by]], ...))

  # if there are results, put them in the ARD format ---------------------------
  if (!is.null(lst_wilcox[["result"]])) {
    tidy_wilcoxtest <- broom::tidy(lst_wilcox[["result"]])

    # add the confidence level if it's reported
    if (!is.null(attr(lst_wilcox[["result"]], "conf.level"))) {
      tidy_wilcoxtest$conf.level <- attr(lst_wilcox[["result"]][["conf.int"]], "conf.level")
    }

    ret <-
      tidy_wilcoxtest |>
      dplyr::mutate(
        dplyr::across(everything(), .fns = list),
        group1 = by,
        variable = variable
      ) |>
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
