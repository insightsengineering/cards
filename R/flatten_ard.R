#' Flatten ARD
#'
#' @description
#' This function ingests an ARD object and flattens it for a readable print of the data.
#'
#' THIS FUNCTION WILL NEED UPDATES AS SOME FLAVORS OF ARD MAY BECOME MORE COMPLEX.
#' List columns are converted to character vectors. If a list column contains
#' elements are cannot easily be coerced to a character vector, the function
#' will fail.
#'
#' @param x an ARD object
#'
#' @return data frame
#' @export
#'
#' @examples
#' ard_categorical(mtcars, by = cyl, variables = c("am", "gear")) |>
#'   flatten_ard()
flatten_ard <- function(x) {
  # convert list columns to character for a nicer print
  dplyr::mutate(
    x,
    dplyr::across(
      where(.is_list_column_of_scalars),
      ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist() |> as.character()
    )
  )
}

# predicate fn whether column is a list that can be represented as vector
.is_list_column_of_scalars <- function(x) {
  is.list(x) && all(unlist(lapply(x, FUN = function(x) length(x) == 1L || is.null(x))))
}
