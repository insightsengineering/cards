#' Evaluate and Capture Conditions
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`\cr
#' **`eval_capture_conditions()`**
#'
#' Evaluates an expression while also capturing error and warning conditions.
#' Function always returns a named list `list(result=, warning=, error=)`.
#' If there are no errors or warnings, those elements will be `NULL`.
#' If there is an error, the result element will be `NULL`.
#'
#' Messages are neither saved nor printed to the console.
#'
#' Evaluation is done via [`rlang::eval_tidy()`]. If errors and warnings are produced
#' using the `{cli}` package, the messages are processed with `cli::ansi_strip()`
#' to remove styling from the message.
#'
#' **`captured_condition_as_message()`/`captured_condition_as_error()`**
#'
#' These functions take the result from `eval_capture_conditions()` and return
#' errors or warnings as either messages (via `cli::cli_inform()`) or
#' errors (via `cli::cli_abort()`). These functions handle cases where the
#' condition messages may include curly brackets, which would typically cause
#' issues when processed with the `cli::cli_*()` functions.
#'
#' Functions return the `"result"` from `eval_capture_conditions()`.
#'
#' @inheritParams rlang::eval_tidy
#' @inheritParams cli::cli_abort
#' @param x (`captured_condition`)\cr
#'   a captured condition created by `eval_capture_conditions()`.
#' @param type (`string`)\cr
#'   the type of condition to return. Must be one of `'error'` or `'warning'`.
#' @param message (`character`)\cr
#'   message passed to `cli::cli_inform()` or `cli::cli_abort()`. The condition
#'   being printed is saved in an object named `condition`, which should be
#'   included in this message surrounded by curly brackets.
#' @param call (`environment`)\cr
#'   Execution environment of currently running function. Default is
#'   `get_cli_abort_call()`.
#' @return a named list
#' @name eval_capture_conditions
#'
#' @examples
#' # function executes without error or warning
#' eval_capture_conditions(letters[1:2])
#'
#' # an error is thrown
#' res <- eval_capture_conditions(stop("Example Error!"))
#' res
#' captured_condition_as_message(res)
#'
#' # if more than one warning is returned, all are saved
#' eval_capture_conditions({
#'   warning("Warning 1")
#'   warning("Warning 2")
#'   letters[1:2]
#' })
#'
#' # messages are not printed to the console
#' eval_capture_conditions({
#'   message("A message!")
#'   letters[1:2]
#' })
NULL

#' @rdname eval_capture_conditions
#' @export
eval_capture_conditions <- function(expr, data = NULL, env = caller_env()) {
  # IF WE EVER NEED TO REWORK/DEBUG REVIEW THE ADVANCED R CONDITIONS CHAPTER
  # https://adv-r.hadley.nz/conditions.html#conditions

  # initialize empty list to return
  lst_result <- list(result = NULL, warning = NULL, error = NULL)

  # tryCatch() saves error messages
  # withCallingHandlers() saves the warnings
  # invokeRestart() suppresses the printing of warnings when code is resumed
  tryCatch(
    withCallingHandlers(
      expr = {
        lst_result[["result"]] <-
          suppressMessages(eval_tidy({{ expr }}, data = data, env = env))
      },
      warning = function(w) {
        lst_result[["warning"]] <<-
          # using `c()` to capture all warnings
          c(lst_result[["warning"]], conditionMessage(w) |> cli::ansi_strip())
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      lst_result[["error"]] <<- conditionMessage(e) |> cli::ansi_strip()
    }
  )

  # return named list of results
  lst_result %>%
    structure(., class = c("captured_condition", class(.)))
}

#' @rdname eval_capture_conditions
#' @export
captured_condition_as_message <- function(x,
                                          message =
                                            c("The following {type} occured:",
                                              "x" = "{condition}"
                                            ),
                                          type = c("error", "warning"),
                                          envir = rlang::current_env()) {
  check_class(x, "captured_condition")
  type <- rlang::arg_match(type)

  # if error/warning is empty, return x invisibly
  if (is_empty(x[[type]])) return(x[["result"]]) # styler: off

  condition <- x[[type]]
  cli::cli_inform(message = message, .envir = envir)

  x[["result"]]
}

#' @rdname eval_capture_conditions
#' @export
captured_condition_as_error <- function(x,
                                        message =
                                          c("The following {type} occured:",
                                            "x" = "{condition}"
                                          ),
                                        type = c("error", "warning"),
                                        call = get_cli_abort_call(),
                                        envir = rlang::current_env()) {
  check_class(x, "captured_condition")
  type <- rlang::arg_match(type)

  # if error/warning is empty, return x invisibly
  if (is_empty(x[[type]])) return(x[["result"]]) # styler: off

  condition <- x[[type]]
  cli::cli_abort(message = message, call = call, .envir = envir)
}
