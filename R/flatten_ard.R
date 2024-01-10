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
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return data frame
#' @export
#'
#' @examples
#' ard_categorical(mtcars, by = "cyl", variables = c("am", "gear"))
flatten_ard <- function(x) {
  # check inputs ---------------------------------------------------------------
  check_class(x = x, class = "card")

  # flatten ard table for easier viewing ---------------------------------------
  x |>
    # remove the formatting functions
    dplyr::select(-where(function(x) all(lapply(x, function(y) is.function(y)) |> unlist()))) |>
    # convert list columns to character for a nicer print
    dplyr::mutate(
      across(
        where(.is_list_column_of_scalars),
        ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist() |> as.character()
      )
    ) |>
    dplyr::select(-any_of("context")) |>
    dplyr::relocate(dplyr::starts_with("group"), dplyr::starts_with("variable"),
                    dplyr::any_of(c("stat_name", "stat_label", "statistic")), .before = 0L) %>%
    dplyr::arrange(dplyr::pick(dplyr::any_of("variable")),
                    dplyr::pick(dplyr::matches("^group[0-9]+$")),
                    dplyr::pick(dplyr::matches("^group[0-9]+_level$")))
}

# predicate fn whether column is a list that can be represented as vector
.is_list_column_of_scalars <- function(x) {
  is.list(x) && all(unlist(lapply(x, FUN = function(x) length(x) == 1L || is.null(x))))
}
