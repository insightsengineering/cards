#' Validate ARD environment metadata
#'
#' @keywords internal
#' @noRd
.validate_environment_metadata <- function(x, y, call = get_cli_abort_call()) {
  is_nonempty_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }

  collect_environment_metadata <- function(card) {
    environments <- list()

    register_environment <- function(value, key) {
      label <- if (is_nonempty_string(key)) paste0(key, " environment") else "creation environment"
      environments[[label]] <<- c(environments[[label]], rlang::env_label(value))
    }

    walk_metadata <- function(value, prefix = NULL) {
      if (inherits(value, "environment")) {
        register_environment(value, prefix)
        return(invisible(NULL))
      }

      if (!is.list(value)) {
        return(invisible(NULL))
      }

      element_names <- names(value)
      if (is.null(element_names)) {
        element_names <- rep("", length(value))
      }

      for (index in seq_along(value)) {
        element <- value[[index]]
        element_name <- element_names[[index]]
        next_prefix <- prefix

        if (is_nonempty_string(element_name)) {
          next_prefix <- if (is_nonempty_string(prefix)) {
            paste0(prefix, "$", element_name)
          } else {
            element_name
          }
        }

        walk_metadata(element, next_prefix)
      }

      invisible(NULL)
    }

    attrs <- attributes(card)
    attr_names <- names(attrs)
    if (is.null(attr_names)) {
      attr_names <- rep("", length(attrs))
    }

    for (i in seq_along(attrs)) {
      attr_value <- attrs[[i]]
      attr_name <- attr_names[[i]]
      prefix <- if (is_nonempty_string(attr_name)) attr_name else NULL
      walk_metadata(attr_value, prefix)
    }

    environments
  }

  environments_x <- collect_environment_metadata(x)
  environments_y <- collect_environment_metadata(y)
  has_environment_x <- length(environments_x) > 0L
  has_environment_y <- length(environments_y) > 0L

  if (has_environment_x || has_environment_y) {
    if (!has_environment_x || !has_environment_y) {
      missing_arg <- if (!has_environment_x && has_environment_y) "x" else "y"

      cli::cli_abort(
        c(
          "!" = "Both ARDs must record their creation environment to compare them.",
          "x" = cli::format_inline("{.arg {missing_arg}} does not store its creation environment.")
        ),
        call = call
      )
    }

    all_environment_labels <- union(names(environments_x), names(environments_y))
    mismatching_labels <- character()

    for (label in all_environment_labels) {
      addresses_x <- environments_x[[label]]
      addresses_y <- environments_y[[label]]

      if (is.null(addresses_x) || is.null(addresses_y)) {
        mismatching_labels <- c(mismatching_labels, label)
        next
      }

      addresses_x <- sort(addresses_x)
      addresses_y <- sort(addresses_y)

      if (!identical(addresses_x, addresses_y)) {
        mismatching_labels <- c(mismatching_labels, label)
      }
    }

    mismatching_labels <- unique(mismatching_labels)

    if (!rlang::is_empty(mismatching_labels)) {
      mismatch_detail <- cli::format_inline(
        "Mismatching environment metadata: {.val {mismatching_labels}}.",
        mismatching_labels = mismatching_labels
      )

      cli::cli_abort(
        paste(
          "The input ARDs were created in different environments.",
          mismatch_detail
        ),
        call = call
      )
    }
  }

  invisible(NULL)
}
