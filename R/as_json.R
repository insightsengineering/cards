#' ARD as JSON
#'
#' Convert ARDs to JSON or write to disk as JSON
#'
#' @param x an ARD
#' @param path file on disk
#' @param ... additional conversion arguments, see also `jsonlite::toJSON()`
#'
#' @return a JSON object
#' @name json
#'
#' @examples
#' ard <-
#'   dplyr::bind_rows(
#'     ard_continuous(mtcars, by = cyl, variables = c("mpg", "hp")),
#'     ard_continuous(mtcars, by = c(cyl, gear), variables = c("mpg", "hp"))
#'   )
#'
#' ard_as_json(ard)
NULL

#' @export
#' @rdname json
ard_as_json <- function(x, ...) {
  as_nested_list(x) |> jsonlite::toJSON(...)
}

#' @export
#' @rdname json
write_ard_as_json <- function(x, path, ...) {
  as_nested_list(x) |> jsonlite::write_json(...)
}


#' @export
#' @rdname json
as_nested_list <- function(x) {
  # check in inputs ------------------------------------------------------------
  rlang::check_installed("jsonlite")
  if (!inherits(x, "card")) {
    cli::cli_abort("Argument {.code x} must be class {.cls card}.")
  }

  # construct the nested lists to convert to JSON ------------------------------
  lst_pre_json <-
    seq_len(nrow(x)) |>
    lapply(FUN = function(i) .one_row_ard_to_nested_list(x[i,]))

  # construct nested list that will be converted to JSON -----------------------
  lst_return <- list() # initialize empty list that will be populated with results
  for (i in seq_len(nrow(x))) {
    eval(lst_pre_json[[i]])
  }

  # return nested list result --------------------------------------------------
  lst_return
}

.one_row_ard_to_nested_list <- function(x) {
  df_preparation <-
    x %>%
    # reorder with primary variable first, followed by stratum
    dplyr::select(., all_of(colnames(.) |> sort())) %>%
    dplyr::select(
      any_of(c("variable", "variable_level")), starts_with("strata"),
                  "stat_name", "statistic", "warning", "error" # TODO: we could apply a formatting function and add that here
      ) |>
    # variable levels are originally stored in lists. unlisting here and saving in tibble as a scalar
    dplyr::mutate(
      dplyr::across(
        # TODO: Does the statistic column need to remain in a list for more complex returns?
        .col = where(is.list) & (dplyr::matches("^strata[0-9]+_level$") | any_of(c("variable_level", "statistic"))),
        .fns = function(x) x[[1]]
      )
    ) |>
    # drop columns that are NA
    dplyr::select(-(where(function(x) all(is.na(x))) & (starts_with("strata") | any_of("variable_level"))))

  # create a character string of the code, that we later convert to an expression
  # TODO: converting strings to expressions feels hacky...is there a better way?
  chr_nested_list_specification <-
    df_preparation |>
    dplyr::select(any_of(c("variable", "variable_level")), starts_with("strata"), "stat_name") |>
    as.list() |>
    lapply(FUN = function(x) glue::glue("[[{shQuote(x)}]]")) |>
    unlist() %>%
    paste(collapse = "") %>%
    # 'lst_return' is the name of the nested list that will be converted to JSON
    {paste0("lst_return", .)}

  # creating final expression defining the results within the nested list
  rlang::expr(!!rlang::parse_expr(chr_nested_list_specification) <- !!df_preparation[c("statistic", "warning", "error")] |> as.list())
}


