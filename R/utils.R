#' ARD-flavor of unique()
#'
#' Essentially a wrapper for `unique(x) |> sort()` with `NA` levels removed.
#' For factors, all levels are returned even if they are unobserved.
#' Similarly, logical vectors always return `c(TRUE, FALSE)`, even if
#' both levels are not observed.
#'
#' @param x (`any`)\cr
#'   a vector
#'
#' @return a vector
#' @keywords internal
#'
#' @examples
#' cards:::.unique_and_sorted(factor(letters[c(5, 5:1)], levels = letters))
#'
#' cards:::.unique_and_sorted(c(FALSE, TRUE, TRUE, FALSE))
#'
#' cards:::.unique_and_sorted(c(5, 5:1))
.unique_and_sorted <- function(x, useNA = c("no", "always")) {
  # styler: off
  useNA <- match.arg(useNA)
  # if a factor return a factor that includes the same levels (including unobserved levels)
  if (inherits(x, "factor")) {
    return(
      factor(
        if (useNA == "no") levels(x)
        else c(levels(x), NA_character_),
        levels = levels(x)
      )
    )
  }
  if (inherits(x, "logical")) {
    if (useNA == "no") return(c(TRUE, FALSE))
    else return(c(TRUE, FALSE, NA))
  }

  # otherwise, return a simple unique and sort of the vector
  if (useNA == "no") return(unique(x) |> sort())
  else return(unique(x) |> sort() |> c(NA))
  # styler: on
}


#' Named List Predicate
#'
#' A predicate function to check whether input is a named list and _not_ a data frame.
#'
#' @param x (`any`)\cr
#'   object to check
#'
#' @return a logical
#' @keywords internal
#'
#' @examples
#' cards:::.is_named_list(list(a = 1:3))
.is_named_list <- function(x, allow_df = FALSE) {
  if (isFALSE(allow_df)) {
    return(is.list(x) && is_named(x) && !is.data.frame(x))
  }
  if (isTRUE(allow_df)) {
    return(is.list(x) && is_named(x))
  }
}

#' A list_flatten()-like Function
#'
#' Function operates similarly to `purrr::list_flatten(x, name_spec = "{inner}")`.
#'
#' @param x (named `list`)\cr
#'   a named list
#'
#' @return a named list
#' @keywords internal
#'
#' @examples
#' x <- list(a = 1, b = list(b1 = 2, b2 = 3), c = list(c1 = 4, c2 = list(c2a = 5)))
#'
#' cards:::.purrr_list_flatten(x)
.purrr_list_flatten <- function(x) {
  ret <- list()

  for (i in seq_along(x)) {
    if (.is_named_list(x[[i]])) {
      ret <- append(ret, values = x[[i]])
    } else {
      ret <- append(ret, values = x[i])
    }
  }

  ret
}
