#' Add Calculated Row
#'
#' @description
#' Use this function to add a new statistic row that is a function of the
#' other statistics in an ARD.
#'
#' @param x (`card`)\cr
#'   data frame of class `'card'`
#' @param expr (`expression`)\cr
#'   an expression
#' @param stat_name (`string`)\cr
#'   string naming the new statistic
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Grouping variables to calculate statistics within
#' @param stat_label (`string`)\cr
#'   string of the statistic label. Default is the `stat_name`.
#' @param fmt_fun (`integer`, `function`, `string`)\cr
#'   a function of an integer or string that can be converted to a function with
#'  `alias_as_fmt_fun()`.
#' @param fmt_fn `r lifecycle::badge("deprecated")`
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_continuous(mtcars, variables = mpg) |>
#'   add_calculated_row(expr = max - min, stat_name = "range")
#'
#' ard_continuous(mtcars, variables = mpg) |>
#'   add_calculated_row(
#'     expr =
#'       dplyr::case_when(
#'         mean > median ~ "Right Skew",
#'         mean < median ~ "Left Skew",
#'         .default = "Symmetric"
#'       ),
#'     stat_name = "skew"
#'   )
add_calculated_row <- function(x,
                               expr,
                               stat_name,
                               by = c(all_ard_groups(), all_ard_variables(), any_of("context")),
                               stat_label = stat_name,
                               fmt_fun = NULL,
                               fmt_fn = deprecated()) {
  set_cli_abort_call()
  expr <- enexpr(expr)

  # deprecated args ------------------------------------------------------------
  if (lifecycle::is_present(fmt_fn)) {
    lifecycle::deprecate_soft(
      when = "0.6.1",
      what = "add_calculated_row(fmt_fn)",
      with = "add_calculated_row(fmt_fun)"
    )
    fmt_fun <- fmt_fn
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(expr)
  check_not_missing(stat_name)
  check_class(x, "card")
  check_string(stat_name)
  check_string(stat_label)
  process_selectors(x, by = {{ by }})

  # calculate additional statistics --------------------------------------------
  ard_calculated_stat <-
    x |>
    dplyr::group_by(dplyr::pick(any_of(by))) |>
    dplyr::group_map(
      \(x_subgroup, df_groups) {
        if (any(duplicated(x_subgroup$stat_name))) {
          cli::cli_abort(
            "Duplicate statistics present within {.arg by} groups: {.val {x_subgroup$stat_name[duplicated(x_subgroup$stat_name)]}}",
            call = get_cli_abort_call()
          )
        }

        new_stat <-
          eval_capture_conditions(
            eval_tidy(expr, data = get_ard_statistics(x_subgroup))
          )
        if (!is_empty(new_stat[["error"]])) {
          cli::cli_abort(
            c("There was an error calculating the new statistic. See below:",
              "x" = new_stat[["error"]]
            ),
            call = get_cli_abort_call()
          )
        }

        df_groups |>
          dplyr::mutate(
            stat = list(.env$new_stat[["result"]]),
            stat_name = .env$stat_name,
            stat_label = .env$stat_label,
            fmt_fun = list(.env$fmt_fun %||% ifelse(is.numeric(new_stat[["result"]]), 1L, as.character))
          )
      }
    ) |>
    dplyr::bind_rows()

  # stack passed ARD and new ARD stats -----------------------------------------
  dplyr::bind_rows(
    x,
    ard_calculated_stat
  )
}
