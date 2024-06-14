#' Missing ARD Statistics
#'
#' Compute Analysis Results Data (ARD) for statistics related to data missingness.
#'
#' @inheritParams ard_continuous
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   results are tabulated by **all combinations** of the columns specified.
#'
#' @return an ARD data frame of class 'card'
#' @name ard_missing
#'
#' @examples
#' ard_missing(ADSL, by = "ARM", variables = "AGE")
#'
#' ADSL |>
#'   dplyr::group_by(ARM) |>
#'   ard_missing(
#'     variables = "AGE",
#'     statistic = ~"N_miss"
#'   )
NULL

#' @export
#' @rdname ard_missing
ard_missing <- function(data, ...) {
  check_not_missing(data)
  UseMethod("ard_missing")
}

#' @export
#' @rdname ard_missing
ard_missing.data.frame <- function(data,
                                   variables,
                                   by = dplyr::group_vars(data),
                                   statistic = everything() ~ c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"),
                                   fmt_fn = NULL,
                                   stat_label = everything() ~ default_stat_labels(),
                                   ...) {
  set_cli_abort_call()
  check_dots_used()

  # check inputs ---------------------------------------------------------------
  check_not_missing(variables)

  # process variable inputs ----------------------------------------------------
  process_selectors(data, variables = {{ variables }})

  # convert all variables to T/F whether it's missing --------------------------
  data <- data |>
    dplyr::mutate(
      across(all_of(variables), Negate(is.na))
    )

  process_formula_selectors(
    data[variables],
    statistic = statistic
  )
  fill_formula_selectors(
    data[variables],
    statistic = formals(asNamespace("cards")[["ard_missing.data.frame"]])[["statistic"]] |> eval()
  )
  check_list_elements(
    x = statistic,
    predicate = \(x) is.character(x) && all(x %in% c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss")),
    error_msg = "Elements passed in the {.arg statistic} argument must be one or more of {.val {c('N_obs', 'N_miss', 'N_nonmiss', 'p_miss', 'p_nonmiss')}}"
  )

  # get the summary statistics -------------------------------------------------
  ard_continuous(
    data = data,
    variables = all_of(variables),
    by = {{ by }},
    statistic = lapply(statistic, \(x) missing_summary_fns(x)),
    fmt_fn = fmt_fn,
    stat_label = stat_label
  ) |>
    dplyr::mutate(
      context = "missing"
    )
}

missing_summary_fns <- function(summaries = c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss")) {
  list(
    var_level =
      function(x, stats = summaries) {
        res <- list()

        if (any(c("N_obs", "N_nonmiss", "p_miss", "p_nonmiss") %in% stats)) {
          res[["N_obs"]] <- length(x)
        }
        if (any(c("N_miss", "N_nonmiss", "p_miss") %in% stats)) {
          res[["N_miss"]] <- sum(!x)
        }
        if (any(c("N_nonmiss", "p_nonmiss") %in% stats)) {
          res[["N_nonmiss"]] <- res[["N_obs"]] - res[["N_miss"]]
        }
        if ("p_miss" %in% stats) {
          res[["p_miss"]] <- res[["N_miss"]] / res[["N_obs"]]
        }
        if ("p_nonmiss" %in% stats) {
          res[["p_nonmiss"]] <- res[["N_nonmiss"]] / res[["N_obs"]]
        }

        res
      }
  )
}
