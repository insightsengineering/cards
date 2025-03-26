#' Print ARD Condition Messages
#'
#' Function parses the errors and warnings observed while calculating the
#' statistics requested in the ARD and prints them to the console as messages.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param condition_type (`string`)\cr
#'   indicates how warnings and errors are returned.
#'   Default is `"inform"` where all are returned as messages.
#'   When `"identity"`, errors are returned as errors and warnings as warnings.
#'
#' @return returns invisible if check is successful, throws all condition messages if not.
#' @export
#'
#' @examples
#' # passing a character variable for numeric summary
#' ard_continuous(ADSL, variables = AGEGR1) |>
#'   print_ard_conditions()
print_ard_conditions <- function(x, condition_type = c("inform", "identity")) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, cls = "card")
  condition_type <- rlang::arg_match(condition_type, call = get_cli_abort_call())

  # print condition messages ---------------------------------------------------
  # styler: off
  if ("error" %in% names(x)) .cli_condition_messaging(x, msg_type = "error", condition_type = condition_type)
  if ("warning" %in% names(x)) .cli_condition_messaging(x, msg_type = "warning", condition_type = condition_type)
  # styler: on

  invisible()
}

#' Print Condition Messages Saved in an ARD
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param msg_type (`string`)\cr
#'   message type. Options are `"warning"` and `"error"`.
#'
#' @return returns invisible if check is successful, throws warning/error messages if not.
#' @keywords internal
#'
#' @examples
#' ard <- ard_continuous(
#'   ADSL,
#'   by = ARM,
#'   variables = AGE
#' )
#'
#' cards:::.cli_condition_messaging(ard, msg_type = "error")
.cli_condition_messaging <- function(x, msg_type, condition_type) {
  set_cli_abort_call()

  # filter the ARD for the rows with messages to print
  ard_condition <- x |> dplyr::filter(!map_lgl(.data[[msg_type]], is.null))

  # if no messages, quit the function early
  if (nrow(ard_condition) == 0L) {
    return(invisible())
  }

  # choose the function for color prints for warnings/errors
  cli_color_fun <-
    switch(msg_type,
      "warning" = cli::col_yellow,
      "error" = cli::col_red
    )

  # create a data frame that is one row per message to print
  # also formats the text that will be printed
  ard_msg <-
    ard_condition |>
    dplyr::group_by(dplyr::pick(all_ard_groups(), all_ard_variables(), all_of(msg_type))) |>
    dplyr::group_map(
      function(.x, .y) {
        dplyr::tibble(
          # this column is the messaging for which groups/variable the message appears in
          cli_variable_msg =
            dplyr::select(.y, all_ard_variables("names")) |>
              dplyr::mutate(across(where(is.list), unlist)) |>
              dplyr::slice(1L) |>
              as.list() |>
              .cli_groups_and_variable() |>
              list(),
          cli_group_msg =
            dplyr::select(.y, all_ard_groups()) |>
              dplyr::mutate(across(where(is.list), unlist)) |>
              dplyr::slice(1L) |>
              as.list() |>
              .cli_groups_and_variable() |>
              list(),
          # character vector of all the stat_names the message applies to
          all_stat_names = list(.x$stat_name),
          # grabs the condition message and colors it with the cli color function
          cond_msg = unlist(.y[[msg_type]]) |> lapply(cli_color_fun)
        )
      }
    ) |>
    dplyr::bind_rows()

  # and finally, print the messages
  cli::cli_inform(
    "The following {cli_color_fun(paste0(msg_type, 's'))} were returned during
       {.fun {error_call(get_cli_abort_call()) |> rlang::call_name()}}:"
  )

  # set cli message function
  # styler: off
  if (condition_type == "inform") cli_msg_fn <- cli::cli_inform
  else if (condition_type == "identity" && msg_type == "warning") cli_msg_fn <- cli::cli_warn
  else if (condition_type == "identity" && msg_type == "error") {
    cli_msg_fn <- \(message, ...) cli::cli_abort(message = message, ..., call = get_cli_abort_call())
  }
  # styler: on


  for (i in seq_len(nrow(ard_msg))) {
    cli_msg_fn(
      paste(
        glue::glue(
          "For variable {ard_msg$cli_variable_msg[[i]]} ",
          "{switch(!is.null(ard_msg$cli_group_msg[[i]]), paste0('(', ard_msg$cli_group_msg[[i]], ')')) %||% ''} ",
          "and {{.val {{ard_msg$all_stat_names[[i]]}}}} statistic{{?s}}"
        ),
        "{ard_msg$cond_msg[[i]]}",
        sep = ": "
      ) |>
        stats::setNames(switch(msg_type,
          "warning" = "!",
          "error" = "x"
        ))
    )
  }

  invisible()
}

#' Locate Condition Messages in an ARD
#'
#' Prints a string of all `group##`/`group##_level` column values and
#' `variable` column values where condition messages occur, formatted
#' using glue syntax.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return a string
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
#' cards:::.cli_groups_and_variable(ard)
.cli_groups_and_variable <- function(x) {
  names <- names(x)

  # format the 'values' or levels of the variables
  levels <-
    x[endsWith(names, "_level")] |>
    lapply(\(x) glue::glue("{{.val {{{cli::cli_format(ifelse(is.numeric(x) || is.logical(x), x, as.character(x)))}}}}}"))
  # rename the levels to remove the '_level' suffix
  names(levels) <- sub(pattern = "_level$", replacement = "", x = names(levels))

  # first subset on the variable names
  ret <- x[grepl(x = names, pattern = "^group[0-9]+$|^variable$")] |>
    # add the varname = value where appropriate
    imap(
      \(x, colname) {
        if (rlang::is_empty(levels[[colname]])) {
          return(glue::glue("{{.var {x}}}"))
        }

        glue::glue("{{.code {x} = {levels[[colname]]}}}")
      }
    ) |>
    paste(collapse = ", ")

  if (ret == "") ret <- NULL
  ret
}
