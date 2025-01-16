#' Rename ARD Group Columns
#'
#' Functions for renaming group columns names in ARDs.
#'
#' @param x (`data.frame`)\cr
#'  an ARD data frame of class 'card'.
#' @param shift (`integer`)\cr
#'  an integer specifying how many values to shift the group IDs,
#'  e.g. `shift=-1` renames `group2` to `group1`.
#'
#' @return an ARD data frame of class 'card'
#' @name rename_ard_groups
#'
#' @examples
#' ard <- ard_continuous(ADSL, by = c(SEX, ARM), variables = AGE)
#'
#' # Example 1 ----------------------------------
#' rename_ard_groups_shift(ard, shift = -1)
#'
#' # Example 2 ----------------------------------
#' rename_ard_groups_reverse(ard)
NULL

#' @rdname rename_ard_groups
#' @export
rename_ard_groups_shift <- function(x, shift = -1) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "card")
  check_integerish(shift)

  # create data frame with old names and new names -----------------------------
  df_group_names <-
    .group_names_as_df(x) |>
    dplyr::mutate(
      new_group_id = .data$old_group_id + as.integer(.env$shift),
      new_group_name =
        pmap(
          list(.data$old_group_name, .data$old_group_id, .data$new_group_id),
          \(old_group_name, old_group_id, new_group_id) {
            str_replace(
              old_group_name,
              pattern = paste0("^group", old_group_id),
              replacement = paste0("group", new_group_id)
            )
          }
        ) |>
          as.character()
    )

  # warn about bad names
  if (any(df_group_names$new_group_id < 1L)) {
    cli::cli_inform(c("There are now non-standard group column names:
                    {.val {df_group_names$new_group_name[df_group_names$new_group_id < 1L]}}.",
      "i" = "Is this the shift you had planned?"
    ))
  }

  # rename columns and return ARD ----------------------------------------------
  x |>
    dplyr::rename(!!!deframe(df_group_names[c("new_group_name", "old_group_name")]))
}

#' @rdname rename_ard_groups
#' @export
rename_ard_groups_reverse <- function(x) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "card")

  # if no groups, return ARD unaltered -----------------------------------------
  if (dplyr::select(x, all_ard_groups()) |> names() |> is_empty()) {
    return(x)
  }

  # create data frame with old names and new names -----------------------------
  df_group_names <- .group_names_as_df(x)

  all_obs_ids <- sort(unique(df_group_names$old_group_id))
  df_group_names$new_group_id <-
    dplyr::recode(
      df_group_names$old_group_id,
      !!!set_names(all_obs_ids, rev(all_obs_ids))
    )
  df_group_names$new_group_name <-
    pmap(
      list(df_group_names$old_group_name, df_group_names$old_group_id, df_group_names$new_group_id),
      \(old_group_name, old_group_id, new_group_id) {
        str_replace(
          old_group_name,
          pattern = paste0("^group", old_group_id),
          replacement = paste0("group", new_group_id)
        )
      }
    ) |>
    as.character()

  # rename columns and return ARD ----------------------------------------------
  x |>
    dplyr::rename(!!!deframe(df_group_names[c("new_group_name", "old_group_name")])) |>
    tidy_ard_column_order()
}

.group_names_as_df <- function(x) {
  dplyr::tibble(
    old_group_name = dplyr::select(x, all_ard_groups()) |> names(),
    old_group_id =
      str_extract(.data$old_group_name, "^group[0-9]+") |>
        str_remove("^group") |>
        as.integer()
  )
}
