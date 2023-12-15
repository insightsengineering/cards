
#' Shuffle ARD
#'
#' This function ingests an ARD object and shuffles the information to prepare for analysis. Helpful for streamlining across multiple ARDs. Combines each group/group_level into 1 column, backfills missing grouping values from the variable levels where possible, and optionally trims statistics-level metadata.
#'
#' @param x an ARD object
#' @param trim Boolean representing whether or not to trim away statistic-level metadata and filter only on numeric statistic values.
#'
#' @return data frame
#' @export
#'
#' @examples
#' bind_ard(
#'   ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
#'   ard_categorical(ADSL, variables = "ARM")
#' ) |>
#' shuffle_ard()
#'
shuffle_ard <- function(x, trim = TRUE){

  check_class_data_frame(x)

  # make sure columns are in order & add index for retaining order
  dat_cards <- x |>
    dplyr::relocate(dplyr::starts_with("group"), dplyr::starts_with("variable"),
                    dplyr::any_of(c("stat_name", "stat_label", "statistic")), .before = 0L) %>%
    dplyr::arrange(dplyr::pick(dplyr::any_of("variable")),
                   dplyr::pick(dplyr::matches("^group[0-9]+$")),
                   dplyr::pick(dplyr::matches("^group[0-9]+_level$"))) |>
    dplyr::mutate(.cards_idx = dplyr::row_number())

  # fill stat label if missing
  dat_cards <- dat_cards |>
    dplyr::mutate(dplyr::across(any_of("stat_label"), ~ dplyr::coalesce(.x, stat_name)))

  # split up the data into data/variable info & cards info
  vars_ard <- dat_cards |> dplyr::select(all_ard_groups(), all_ard_variables()) |> names()
  vars_protected <- setdiff(names(dat_cards), vars_ard)

  dat_cards_grps <- dat_cards |>
    dplyr::select(-all_of(vars_protected), ".cards_idx")

  dat_cards_stats <- dat_cards |>
    dplyr::select(all_of(vars_protected))

  # process the data/variable info
  dat_cards_grps_processed <- dat_cards_grps |>
    # unlist the list-columns
    dplyr::mutate(
      dplyr::across(
        where(.is_list_column_of_scalars),
        ~lapply(., \(x) if (!is.null(x)) x else NA_character_) |> unlist()
      )
    ) |>
    .check_var_nms(vars_protected = names(dat_cards_stats)) |>
    .rnm_grp_vars() |>
    .fill_grps_from_variables()

  # join together again
  dat_cards_out <- dplyr::left_join(
    dat_cards_grps_processed,
    dat_cards_stats,
    by = ".cards_idx"
  )

  # fill stat_label -> variable_level if exists
  if ("stat_label" %in% names(dat_cards_out)){
    dat_cards_out <- dat_cards_out |>
      dplyr::mutate(dplyr::across(any_of("variable_level"), ~dplyr::coalesce(.x, stat_label)))
  }

  dat_cards_out <- dat_cards_out |>
    dplyr::rename(any_of(c(label="variable_level"))) |>
    dplyr::arrange(".cards_idx")   |>
    dplyr::select(-".cards_idx")

  if (trim){
    dat_cards_out |> .trim_ard()
  } else {
    dat_cards_out
  }
}


# trim extra variables and subset to numeric results only
.trim_ard <- function(x) {

  check_class_data_frame(x)

  # flatten ard table for easier viewing ---------------------------------------
  x |>
    # remove the formatting functions / warning / error
    dplyr::select(-where(function(x) all(lapply(x, function(y) is.null(y) || is.function(y)) |> unlist())),
                  -"warning", -"error",
                  -any_of(c("stat_label"))) |>
    # filter to numeric statistic values
    dplyr::filter(map_lgl(.data$statistic, is.numeric)) |>
    # unlist the list-columns
    dplyr::mutate(
      dplyr::across(
        where(.is_list_column_of_scalars),
        ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist() |> unname()
      )
    )
}

# check all group and variable names against protected names,
#  and modify to be unique if needed
.check_var_nms <- function(x, vars_protected){

  # get all represented variable names from original data
  var_nms <- x |>
    dplyr::select(-ends_with("_level"), -".cards_idx") |>
    unlist(use.names = FALSE) |>
    unique()

  # create uniqueness across all variables from original data & cards-specific
  # variables
  var_nms_new <- make.unique(c(vars_protected, var_nms)) |>
    utils::tail(n = length(var_nms)) |>
    set_names(var_nms)

  # subset to only the ones needing recoding
  var_nms_new <- var_nms_new[imap(var_nms_new,
                                  function(x,y){
                                    if (is.na(x)) FALSE else !x == y}
  ) |>
    unlist(use.names = FALSE)]

  # perform recodes if needed
  if (length(var_nms_new)>0){
    x |>
      dplyr::mutate(dplyr::across(-c(ends_with("_level"), ".cards_idx"),
                                  ~ dplyr::recode(.x, !!!var_nms_new)))
  } else {
    x
  }

}

.capture_grp_vars <- function(x){

  grp_vars <- names(x)[grep("^group[0-9]+$", names(x))]

  x |>
    dplyr::rowwise() |>
    dplyr::mutate(.cards_grp_vars = list(dplyr::c_across(any_of(grp_vars))))
}

# function to rename group variables
.rnm_grp_vars <- function(x){

  grp_var_levs <- names(x)[grep("^group[0-9]+_level$", names(x))]
  grp_vars <- names(x)[grep("^group[0-9]+$", names(x))]

  if (length(grp_vars)==0){
    return(x)
  }

  # loop through each of the grouping variables
  for (v in grp_vars){

    # rename as the variable level within the unique levels of the grouping variable
    x <- x |>
      dplyr::mutate(!!v := forcats::fct_inorder(.data[[v]])) |>
      dplyr::group_by(.data[[v]]) |>
      dplyr::group_split()|>
      map(function(dat){

        v_lev <- paste0(v, "_level")
        v_new <- unique(dat[[v]]) |> as.character()

        # drop if all missing
        if (is.na(v_new) | all(is.na(dat[[v_lev]]))) {
          dplyr::select(dat, -all_of(c(v_lev, v)))
        } else {

          #rename _level var & drop source
          dat %>%
            dplyr::rename(!!v_new := all_of(v_lev)) |>
            dplyr::select(-all_of(v))
        }
      }) |>
      dplyr::bind_rows()
  }

  x |>
    dplyr::relocate(any_of(c("variable","variable_level",".cards_idx")), .after = last_col())
}

# back fill grp variables if any variables match them
.fill_grps_from_variables <- function(x){

  # within each variable, check if there is a match against one of the grouping cols
  # if the corresponding value in that grouping col is missing, backfill with the variable level
  x %>%
    dplyr::mutate(variable = forcats::fct_inorder(.data$variable)) |>
    dplyr::group_by(.data$variable) |>
    dplyr::group_split() |>
    map(function(dat){
      grp_match <- names(dat)[names(dat)==unique(dat$variable)]
      if(length(grp_match)>0 && "variable_level" %in% names(dat)){
        dat |>
          dplyr::mutate(!!grp_match := ifelse(is.na(.data[[grp_match]]),
                                            .data$variable_level,
                                            .data[[grp_match]]))
      } else {
        dat
      }
    }) |>
    dplyr::bind_rows()
}


