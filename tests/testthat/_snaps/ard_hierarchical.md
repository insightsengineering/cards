# ard_hierarchical() works without by variables

    Code
      class(ard_heir_no_by)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_hierarchical() works without any variables

    Code
      ard_hierarchical(data = ADAE, variables = starts_with("xxxx"), by = c(TRTA,
        AESEV))
    Output
      # A tibble: 0 x 0

# ard_hierarchical_count() works without by variables

    Code
      class(ard_heir_no_by)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_hierarchical_count() works without any variables

    Code
      ard_hierarchical_count(data = ADAE, variables = starts_with("xxxx"), by = c(
        TRTA, AESEV))
    Output
      # A tibble: 0 x 0

