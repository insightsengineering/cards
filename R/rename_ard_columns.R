

rename_ard_columns <- function(x, columns = c("group", "variable"), fill = "<overall>") {
  # process inputs -------------------------------------------------------------
  check_class(x, "card")
  columns <- arg_match(columns, multiple = TRUE)

  # create two vectors of paired names/levels ----------------------------------
  columns_with_names <- character()
  columns_with_levels <- character()
  if ("group" %in% columns) {
    columns_with_names <- dplyr::select(x, all_ard_groups("names")) |> names()
    columns_with_levels <- paste0(columns_with_names, "_level")
  }
  if ("variable" %in% columns) {
    columns_with_names <- c(columns_with_names, "variable")
    columns_with_levels <- c(columns_with_levels, "variable_level")
  }

  # rename the columns ---------------------------------------------------------
  x <-
    dplyr::group_by(x, across(all_of(columns_with_names))) |>
    dplyr::group_map(
      function(df_ard, df_groups) {
        # populate 'fill' values if the 'level' column is not present or it is all NA
        for (i in seq_along(columns_with_names)) {



        }
      }
    )


    for (i in seq_along(columns_with_names)) {

  }


  if ("group" %in% columns) {
    # number of groups
    group_n <- dplyr::select(x, all_ard_groups("names")) |>
      names() |>
      str_remove_all("^group") |>
      as.integer() |>
      max()

    x <-
      dplyr::group_by(x, across(all_ard_groups())) |>
      dplyr::group_map(
        function(df_ard, df_groups) {
          # populate 'fill' values if the 'level' column is not present or NA
          for (i in seq_len(group_n)) {
            if (!paste0("group", i, "_level") %in% names(df_groups) ||
                is.na(df_groups[[paste0("group", i, "_level")]])) {
              df_groups <- df_groups |>
                dplyr::mutate(
                  "{paste0('group', i, '_level')}" := list(fill),
                  .after = paste0("group", i)
                )
            }
          }

          # unlist the levels column
          df_groups <- df_groups |>
            dplyr::mutate(across(all_ard_groups("levels"), unlist))

          # rename the levels column and drop the group## column
          for (i in seq_len(group_n)) {
            browser()
            df_groups <-
              df_groups |>
              dplyr::rename(
                "{df_groups[[paste0('group', i)]]}" := glue::glue("{paste0('group', i, '_level')}")
              ) |>
              dplyr::select(-glue::glue("{paste0('group', i)}"))
          }

          # last bind the columns together
          dplyr::bind_cols(df_groups, df_ard)
        }
      ) |>
      dplyr::bind_rows()
  }

  if ("variable" %in% columns) {
    x <-
      dplyr::group_by(x, across(all_ard_variables())) |>
      dplyr::group_map(
        function(df_ard, df_variables) {
          # populate 'fill' values if the 'level' column is not present or NA
          if (!"variable_level" %in% names(df_variables) ||
              is.na(df_variables[["variable_level"]])) {
            df_variables <- df_variables |>
              dplyr::mutate(
                variable_level := list(fill),
                .after = "variable"
              )
          }

          # unlist the levels column
          df_variables <- df_variables |>
            dplyr::mutate(across(all_ard_variables("levels"), unlist))

          # rename the levels column and drop the group## column
          df_variables <-
            df_variables |>
            dplyr::rename(
              "{df_variables[['variable']]}" := "variable_level"
            ) |>
            dplyr::select(-"variable")

          # last bind the columns together
          dplyr::bind_cols(df_variables, df_ard)
        }
      ) |>
      dplyr::bind_rows()
  }

  x
}
