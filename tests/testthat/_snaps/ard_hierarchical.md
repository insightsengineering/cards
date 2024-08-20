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
      1   TRTA      Placebo  AESEV         MILD  AESOC    GASTROIN…  AEDECOD
        variable_level stat_name stat_label stat
      1      ABDOMINA…         n          n    0
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
      1   TRTA      Placebo  AESEV         MILD  AESOC    GASTROIN…  AEDECOD
        variable_level stat_name stat_label stat
      1      ABDOMINA…         n          n    0
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

