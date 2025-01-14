# ard_hierarchical() works without by variables

    Code
      class(ard_heir_no_by)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_hierarchical() works without any variables

    Code
      ard_hierarchical(data = ADAE, variables = starts_with("xxxx"), by = c(TRTA,
        AESEV))
    Message
      {cards} data frame: 0 x 0
    Output
      data frame with 0 columns and 0 rows

# ard_hierarchical(id) argument works

    Code
      head(ard_hierarchical(data = ADAE, variables = c(AESOC, AEDECOD), by = c(TRTA,
        AESEV), denominator = dplyr::rename(ADSL, TRTA = ARM), id = USUBJID), 1L)
    Condition
      Warning:
      Duplicate rows found in data for the "USUBJID" column.
      i Percentages/Denominators are not correct.
    Message
      {cards} data frame: 1 x 15
    Output
        group1 group1_level group2 group2_level group3 group3_level variable
      1   TRTA      Placebo  AESEV         MILD  AESOC    CARDIAC …  AEDECOD
        variable_level stat_name stat_label stat
      1      ATRIAL F…         n          n    0
    Message
      i 4 more variables: context, fmt_fn, warning, error

---

    Code
      head(ard_hierarchical(data = ADAE, variables = c(AESOC, AEDECOD), by = c(TRTA,
        AESEV), denominator = dplyr::rename(ADSL, TRTA = ARM), id = c(USUBJID, SITEID)),
      1L)
    Condition
      Warning:
      Duplicate rows found in data for the "USUBJID" and "SITEID" columns.
      i Percentages/Denominators are not correct.
    Message
      {cards} data frame: 1 x 15
    Output
        group1 group1_level group2 group2_level group3 group3_level variable
      1   TRTA      Placebo  AESEV         MILD  AESOC    CARDIAC …  AEDECOD
        variable_level stat_name stat_label stat
      1      ATRIAL F…         n          n    0
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_hierarchical_count() works without by variables

    Code
      class(ard_heir_no_by)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_hierarchical_count() works without any variables

    Code
      ard_hierarchical_count(data = ADAE, variables = starts_with("xxxx"), by = c(
        TRTA, AESEV))
    Message
      {cards} data frame: 0 x 0
    Output
      data frame with 0 columns and 0 rows

# ard_hierarchical() errors with incomplete factor columns

    Code
      ard_hierarchical(dplyr::mutate(mtcars, am = factor(am, levels = character(0))),
      variables = c(vs, am))
    Condition
      Error in `ard_hierarchical()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_hierarchical(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1, NA),
      exclude = NULL)), variables = c(vs, am))
    Condition
      Error in `ard_hierarchical()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

# ard_hierarchical_count() errors with incomplete factor columns

    Code
      ard_hierarchical_count(dplyr::mutate(mtcars, am = factor(am, levels = character(
        0))), variables = c(vs, am))
    Condition
      Error in `ard_hierarchical_count()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_hierarchical_count(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1,
        NA), exclude = NULL)), variables = c(vs, am))
    Condition
      Error in `ard_hierarchical_count()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

