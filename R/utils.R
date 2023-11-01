#' ARD-flavor of unique()
#'
#' Essentially a wrapper for `unique(x) |> sort()` with NA levels removed.
#' Expect in the case of factor and logical vectors.
#' For factors, all levels are returned even if they are unobserved.
#' Similarly, logical vectors always return `c(TRUE, FALSE)`, even if
#' both levels are not observed.
#'
#' @param x a vector
#' @return a vector
#' @noRd
.unique_and_sorted <- function(x) {
  # if a factor return a factor that includes the same levels (including unobserved levels)
  if (inherits(x, "factor")) {
    return(factor(levels(x), levels = levels(x)))
  }
  if (inherits(x, "logical")) {
    return(c(TRUE, FALSE))
  }

  # otherwise, return a simple unique and sort of the vector
  unique(x) |> sort()
}


#' Named list predicate
#'
#' A predicate function whether input is a named list and _not_ a data frame.
#'
#' @param x object to check
#' @keywords internal
#' @noRd
.is_named_list <- function(x, allow_df = FALSE) {
  if (isFALSE(allow_df))
    return(is.list(x) && rlang::is_named(x) && !is.data.frame(x))
  if (isTRUE(allow_df))
    return(is.list(x) && rlang::is_named(x))
}

#' A list_flatten()-like function
#'
#' Function operates similarly to `purrr::list_flatten(x, name_spec = "{inner}")`
#'
#' @param x a named list
#' @keywords internal
#' @noRd
.purrr_list_flatten <- function(x) {
  ret <- list()

  for (i in seq_along(x)) {
    if (.is_named_list(x[[i]])) ret <- append(ret, values = x[[i]])
    else ret <- append(ret, values = x[i])
  }

  ret
}

