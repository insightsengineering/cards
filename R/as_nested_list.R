#' ARD as Nested List
#'
#' `r lifecycle::badge('experimental')`\cr
#' Convert ARDs to nested lists.
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#'
#' @return a nested list
#' @export
#'
#' @examples
#' ard_continuous(mtcars, by = "cyl", variables = c("mpg", "hp")) |>
#'   as_nested_list()
as_nested_list <- function(x) {
  set_cli_abort_call()

  # check in inputs ------------------------------------------------------------
  check_class(x, cls = "card")

  # format/round the statistics, if not already done ---------------------------
  if (!"stat_fmt" %in% names(x)) {
    x <- apply_fmt_fun(x)
  }

  # construct the nested lists to convert to JSON ------------------------------
  lst_pre_json <-
    seq_len(nrow(x)) |>
    lapply(FUN = function(i) .one_row_ard_to_nested_list(x[i, ]))

  # construct nested list that will be converted to JSON -----------------------
  lst_return <- list() # initialize empty list that will be populated with results
  for (i in seq_len(nrow(x))) {
    eval(lst_pre_json[[i]])
  }

  # return nested list result --------------------------------------------------
  lst_return
}


#' Convert One Row to Nested List
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card' with one row
#'
#' @return an expression that represents an element of a nested list
#' @keywords internal
#'
#' @examples
#' ard_continuous(mtcars, variables = mpg) |>
#'   dplyr::filter(dplyr::row_number() %in% 1L) |>
#'   apply_fmt_fun() |>
#'   cards:::.one_row_ard_to_nested_list()
.one_row_ard_to_nested_list <- function(x) {
  df_preparation <-
    x |>
    # variable levels are originally stored in lists. unlisting here and saving in tibble as a scalar
    dplyr::mutate(
      across(
        # TODO: Does the statistic column need to remain in a list for more complex returns?
        .cols = where(is.list) & (dplyr::matches("^group[0-9]+_level$") | any_of("variable_level")),
        .fns = function(x) x[[1]]
      )
    ) %>%
    # reorder with primary variable first, followed by stratum
    dplyr::select(., all_of(colnames(.) |> sort())) %>% # styler: off
    dplyr::select(
      any_of(c("variable", "variable_level")), starts_with("group"),
      "stat_name", "stat", "stat_fmt", "warning", "error", "context" # TODO: we could apply a formatting function and add that here
    ) |>
    # drop columns that are NA
    dplyr::select(-(where(function(x) all(is.na(x))) & (starts_with("group") | any_of("variable_level"))))

  # create a character string of the code, that we later convert to an expression
  # TODO: converting strings to expressions feels hacky...is there a better way?
  chr_nested_list_specification <-
    df_preparation |>
    dplyr::select(any_of(c("variable", "variable_level")), starts_with("group"), "stat_name") |>
    as.list() |>
    imap(function(x, y) glue::glue("[[{shQuote(y)}]][[{shQuote(x)}]]")) |>
    unlist() %>%
    paste(collapse = "") %>%
    # 'lst_return' is the name of the nested list that will be converted to JSON
    {paste0("lst_return", .)} # styler: off

  # creating final expression defining the results within the nested list
  expr(
    !!parse_expr(chr_nested_list_specification) <-
      !!dplyr::select(
        df_preparation,
        any_of(c("stat", "stat_fmt", "warning", "error", "context"))
      ) |>
        # this essentially flattens the nested list one level, while maintaining the names
        imap(function(x, y) x[[1]])
  )
}
