#' ARD as Nested List
#'
#' Convert ARDs to Nested Lists
#'
#' @param x an ARD
#'
#' @return a nested list
#' @export
#'
#' @examples
#' ard <- ard_continuous(mtcars, by = "cyl", variables = c("mpg", "hp"))
#'
#' as_nested_list(ard)
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
    x |>
    # variable levels are originally stored in lists. unlisting here and saving in tibble as a scalar
    dplyr::mutate(
      dplyr::across(
        # TODO: Does the statistic column need to remain in a list for more complex returns?
        .col = where(is.list) & (dplyr::matches("^group[0-9]+_level$") | any_of("variable_level")),
        .fns = function(x) x[[1]]
      ),
      statistic_fmt =
        .map2(
          .data$statistic,
          .data$statistic_fmt_fn,
          function(x, fn) if (!is.null(fn)) fn(x) else NULL
        )
    ) %>%
    # reorder with primary variable first, followed by stratum
    dplyr::select(., all_of(colnames(.) |> sort())) %>%
    dplyr::select(
      any_of(c("variable", "variable_level")), starts_with("group"),
      "stat_name", "statistic", "statistic_fmt", "warning", "error", "context" # TODO: we could apply a formatting function and add that here
    ) |>
    # drop columns that are NA
    dplyr::select(-(where(function(x) all(is.na(x))) & (starts_with("group") | any_of("variable_level"))))

  # create a character string of the code, that we later convert to an expression
  # TODO: converting strings to expressions feels hacky...is there a better way?
  chr_nested_list_specification <-
    df_preparation |>
    dplyr::select(any_of(c("variable", "variable_level")), starts_with("group"), "stat_name") |>
    as.list() |>
    .imap(function(x, y) glue::glue("[[{shQuote(y)}]][[{shQuote(x)}]]")) |>
    # lapply(FUN = function(x) glue::glue("[[{shQuote(x)}]]")) |>
    unlist() %>%
    paste(collapse = "") %>%
    # 'lst_return' is the name of the nested list that will be converted to JSON
    {paste0("lst_return", .)}

  # creating final expression defining the results within the nested list
  rlang::expr(
    !!rlang::parse_expr(chr_nested_list_specification) <-
      !!dplyr::select(
        df_preparation,
        any_of(c("statistic", "statistic_fmt", "warning", "error", "context"))
      ) |>
      # this essentially flattens the nested list one level, while maintaining the names
      .imap(function(x, y) x[[1]])
  )
}


