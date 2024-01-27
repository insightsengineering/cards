#' Print ARD Condition Messages
#'
#' Function parses the errors and warnings observed while calculating the
#' statistics requested in the ARD and prints them to the console as messages.
#'
#' @param x (`card`)\cr
#'   an ARD data frame
#'
#' @return NULL
#' @export
#'
#' @examples
#' ard_continuous(
#'   ADSL,
#'   by = ARM,
#'   variables = AGE,
#'   statistics = ~ list(
#'     mean = \(x) mean(x),
#'     mean_warning = \(x) {
#'       warning("warn1")
#'       warning("warn2")
#'       mean(x)
#'     },
#'     err_fn = \(x) stop("'tis an error")
#'   )
#' ) |>
#'   print_ard_conditions()
print_ard_conditions <- function(x) {
  check_class(x, class = "card")

  # print condition messages ---------------------------------------------------
  .cli_condition_messaging(x, msg_type = "error")
  .cli_condition_messaging(x, msg_type = "warning")

  invisible()
}

# this function prints either the warnings or errors saved in the ARD
.cli_condition_messaging <- function(x, msg_type) {
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
            dplyr::select(.y, all_ard_variables("variables")) |>
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
  cli::cli_inform("The following {cli_color_fun(paste0(msg_type, 's'))} were returned while calculating statistics:")
  for (i in seq_len(nrow(ard_msg))) {
    cli::cli_inform(c(
      glue::glue(
        "For variable {ard_msg$cli_variable_msg[[i]]} ",
        "{switch(!is.null(ard_msg$cli_group_msg[[i]]), paste0('(', ard_msg$cli_group_msg[[i]], ')')) %||% ''} ",
        "and {{.val {{ard_msg$all_stat_names[[i]]}}}} statistic{{?s}}: ",
        "{ard_msg$cond_msg[[i]]}"
      ) |>
        stats::setNames(switch(msg_type,
          "warning" = "!",
          "error" = "x"
        ))
    ))
  }

  invisible()
}

.cli_groups_and_variable <- function(x) {
  names <- names(x)

  # format the 'values' or levels of the variables
  levels <-
    x[endsWith(names, "_level")] |>
    lapply(\(x) glue::glue("{{.val {{{cli::cli_format(x)}}}}}"))
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
