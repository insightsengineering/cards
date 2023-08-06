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
  dplyr::mutate(x, dplyr::across(where(is.list), ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist() |> as.character()))
}
