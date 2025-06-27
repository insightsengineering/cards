#' Rename ARD Variables
#'
#' Rename the grouping and variable columns to their original column names.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to rename, e.g. selecting columns `c('group1', 'group2', 'variable')`
#'   will rename `'group1_level'` to the name of the variable found in `'group1'`.
#'   When, for example, the `'group1_level'` does not exist, the values of the
#'   new column are filled with the values in the `fill` argument.
#'   Default is `c(all_ard_groups("names"), all_ard_variables("names"))`.
#' @param fill (scalar/glue)\cr
#'   a scalar to fill column values when the variable does not have levels.
#'   If a character is passed, then it is processed with `glue::glue()`
#'   where the `colname` element is available to inject into the string,
#'   e.g. `'Overall {colname}'` may resolve to `'Overall AGE'` for an AGE column.
#'   Default is `'{colname}'`.
#' @param unlist `r lifecycle::badge("deprecated")`
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' ADSL |>
#'   ard_categorical(by = ARM, variables = AGEGR1) |>
#'   apply_fmt_fun() |>
#'   rename_ard_columns() |>
#'   unlist_ard_columns()
#'
#' # Example 2 ----------------------------------
#' ADSL |>
#'   ard_continuous(by = ARM, variables = AGE) |>
#'   apply_fmt_fun() |>
#'   rename_ard_columns(fill = "Overall {colname}") |>
#'   unlist_ard_columns()
rename_ard_columns <- function(x,
                               columns = c(all_ard_groups("names"), all_ard_variables("names")),
                               fill = "{colname}", unlist = NULL) {
  # check inputs ---------------------------------------------------------------
  if (!missing(unlist)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "cards::rename_ard_columns(unlist)",
      with = "unlist_ard_columns()",
      details = "Argument has been ignored."
    )
  }
  set_cli_abort_call()
  check_not_missing(x)
  check_class(x, "card")
  process_selectors(x, columns = {{ columns }})
  check_scalar(fill)
  if (!is_empty(setdiff(columns, dplyr::select(x, all_ard_groups("names"), all_ard_variables("names")) |> names()))) {
    bad_columns <-
      setdiff(columns, dplyr::select(x, all_ard_groups("names"), all_ard_variables("names")) |> names())
    cli::cli_abort(
      c("The {.arg column} argument may only select columns using {.code all_ard_groups(\"names\")}
       and {.code all_ard_variables(\"names\")}",
        "i" = "{cli::qty(bad_columns)} Column{?s} {.val {bad_columns}} {?is/are} not a valid selection."
      ),
      call = get_cli_abort_call()
    )
  }

  # separate selected names from levels
  column_names <- x |>
    dplyr::select(
      intersect(
        c(all_ard_groups("names"), all_ard_variables("names")),
        all_of(columns)
      )
    ) |>
    names()
  all_new_names <- x[column_names] |>
    unlist() |>
    unique() |>
    discard(is.na) |>
    unname()

  if (any(all_new_names %in% names(x))) {
    protected_names <- all_new_names[all_new_names %in% names(x)]
    cli::cli_abort(
      "New column name(s) {.val {protected_names}} cannot be added, because they are already present.",
      call = get_cli_abort_call()
    )
  }

  x |>
    dplyr::mutate(...ard_row_order... = dplyr::row_number()) |>
    dplyr::group_by(dplyr::pick(all_of(column_names))) |>
    dplyr::group_map(
      \(df, df_group) {
        lst_group <- as.list(df_group) |> discard(is.na)
        names_group <- names(lst_group)

        # cycle over all columns
        for (v in names_group) {
          # if level column does not exist, adding it
          if (!paste0(v, "_level") %in% names(df)) {
            df[[paste0(v, "_level")]] <- list(NULL)
          }

          fill_glued <-
            case_switch(
              is.character(fill) ~
                glue::glue_data(.x = lst_group[v] |> set_names("colname"), fill) |> as.character(),
              .default = fill
            )

          # replace null values
          df[[lst_group[[v]]]] <-
            df[[paste0(v, "_level")]] |>
            map(~ .x %||% fill_glued)
          df[[paste0(v, "_level")]] <- NULL
        }

        df |> dplyr::select(-any_of(c(columns, paste0(columns, "_level"))))
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(!!sym("...ard_row_order...")) |>
    dplyr::relocate(all_of(all_new_names), .before = 1L) |>
    dplyr::select(-"...ard_row_order...") |>
    dplyr::mutate(
      # replace NULL values with NA, then unlist
      across(all_of(all_new_names), ~ map(., \(value) value %||% NA) |> unlist())
    )
}
