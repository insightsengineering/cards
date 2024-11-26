# ard_pairwise(variable) messaging

    Code
      ard_pairwise(ADSL, variable = c(ARM, AGEGR1), .f = function(df) ard_complex(df,
        variables = AGE, statistic = ~ list(ttest = ttest_fn)))
    Condition
      Error in `ard_pairwise()`:
      ! The `variable` argument must be length 1.

---

    Code
      ard_pairwise(ADSL, variable = NOT_A_COLUMN, .f = function(df) ard_complex(df,
        variables = AGE, statistic = ~ list(ttest = ttest_fn)))
    Condition
      Error in `ard_pairwise()`:
      ! Error processing `variable` argument.
      ! Can't select columns that don't exist. x Column `NOT_A_COLUMN` doesn't exist.
      i Select among columns "STUDYID", "USUBJID", "SUBJID", "SITEID", "SITEGR1", "ARM", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "TRTDUR", "AVGDD", "CUMDOSE", "AGE", "AGEGR1", "AGEGR1N", ..., "DCREASCD", and "MMSETOT"

# ard_pairwise(.f) messaging

    Code
      ard_pairwise(ADSL, variable = ARM, .f = function(df) stop("I MADE THIS ERROR"))
    Condition
      Error in `ard_pairwise()`:
      ! The following error occurred for 'Placebo' vs. 'Xanomeline High Dose'. See message below.
      x I MADE THIS ERROR

# ard_pairwise(include) messaging

    Code
      ard_pairwise(ADSL, variable = ARM, .f = function(df) ard_complex(df, variables = AGE,
        statistic = ~ list(ttest = ttest_fn)), include = "NOT_A_LEVEL")
    Condition
      Error in `ard_pairwise()`:
      ! The `include` argument must be NULL or one or more of "Placebo", "Xanomeline High Dose", and "Xanomeline Low Dose".

---

    Code
      ard_pairwise(ADSL, variable = ARM, .f = function(df) ard_complex(df, variables = AGE,
        statistic = ~ list(ttest = ttest_fn)), include = mtcars)
    Condition
      Error in `ard_pairwise()`:
      ! The `include` argument must be a simple vector, not a data frame.

