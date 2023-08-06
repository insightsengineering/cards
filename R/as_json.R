

as_json <- function(x) {
  # check in inputs ------------------------------------------------------------
  rlang::check_installed("jsonlite")
  if (!inherits(x, "card")) {
    cli::cli_abort("Argument {.code x} must be class {.cls card}.")
  }

  # get the number of strata variables -----------------------------------------
  n_strata <-
    dplyr::select(x, dplyr::matches("^strata[0-9]+$")) |>
    colnames() |>
    gsub(pattern = "^strata", replacement = "", x = _) |>
    union("0") |>
    as.integer() |>
    max()

  # construct the nested lists to convert to JSON ------------------------------

}

# ard <- ard_continuous(mtcars, by = cyl, variables = c("mpg", "hp"))
# ard
#
# ard %>%
#   # dplyr::mutate(., lst_statistic = .data_frame_to_named_list(.[c("stat_name", "statistic")])) |>
#   dplyr::select(any_of(c("variable_level", "variable")), starts_with("strata"), all_of(c("stat_name", "statistic"))) |>
#   dplyr::filter(dplyr::row_number() %in% 1L) |>
#   dplyr::mutate(statistic = statistic[[1]]) |>
#   as.list() |>
#   lapply(FUN = function(x) glue::glue("[[{shQuote(x)}]]"))
#
#
# list(mpg =
#        list(cyl =
#               list("4" =
#                      list("N" = 11))))
#
# ttt <- list()
# ttt[["mpg"]][["cyl"]][["4"]][["N"]] <- 11
# ttt[["mpg"]][["cyl"]][["4"]][["mean"]] <- 4.5
# ttt |> jsonlite::toJSON()
# ttt[["hp"]][["cyl"]][["4"]][["N"]] <- 11
