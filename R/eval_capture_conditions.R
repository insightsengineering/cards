#' Evaluate and Capture Conditions
#'
#' @description
#' Evaluates an expression while also capturing error and warning conditions.
#' Function always returns a named list `list(result=, warning=, error=)`.
#' If there are no errors or warnings, those elements will be `NULL`.
#' If there is an error, the result element will be `NULL`.
#'
#' Messages are neither saved nor printed to the console.
#'
#' Evaluation is done via [eval_tidy()].
#'
#' @inheritParams rlang::eval_tidy
#' @return a named list
#' @export
#'
#' @examples
#' # function executes without error or warning
#' eval_capture_conditions(letters[1:2])
#'
#' # an error is thrown
#' eval_capture_conditions(stop("Example Error!"))
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
        # TODO: update this `<<-`: I don't think CRAN likes the super assignment
        lst_result[["warning"]] <<-
          # using `c()` to capture all warnings
          c(lst_result[["warning"]], conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      # TODO: update this `<<-`: I don't think CRAN likes the super assignment
      lst_result[["error"]] <<- conditionMessage(e)
    }
  )

  # return named list of results
  lst_result
}
