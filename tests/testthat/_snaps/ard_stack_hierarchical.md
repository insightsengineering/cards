# ard_stack_hierarchical(variables) messaging removed obs

    Code
      ard <- ard_stack_hierarchical(dplyr::mutate(ADAE_small, AESOC = ifelse(dplyr::row_number() ==
        1L, NA, AESOC)), variables = c(AESOC, AEDECOD), id = USUBJID, denominator = dplyr::rename(
        ADSL, TRTA = TRT01A))
    Message
      * Removing 1 row from `data` with NA or NaN values in "AESOC" and "AEDECOD" columns.

---

    Code
      ard <- ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD), id = USUBJID,
      by = TRTA, denominator = dplyr::mutate(dplyr::rename(ADSL, TRTA = TRT01A),
      TRTA = ifelse(dplyr::row_number() == 1L, NA, TRTA)))
    Message
      * Removing 1 row from `denominator` with NA or NaN values in "TRTA" column.

# ard_stack_hierarchical(variables) messaging

    Code
      ard_stack_hierarchical(ADAE_small, variables = starts_with("xxxxx"), id = USUBJID,
      denominator = dplyr::rename(ADSL, TRTA = TRT01A))
    Condition
      Error in `ard_stack_hierarchical()`:
      ! Arguments `variables` and `include` cannot be empty.

---

    Code
      ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD), id = starts_with(
        "xxxxx"), denominator = dplyr::rename(ADSL, TRTA = TRT01A))
    Condition
      Error in `ard_stack_hierarchical()`:
      ! Argument `id` cannot be empty.

# ard_stack_hierarchical(by) messaging

    Code
      ard <- ard_stack_hierarchical(dplyr::mutate(ADAE_small, TRTA = ifelse(dplyr::row_number() ==
        1L, NA, TRTA)), variables = c(AESOC, AEDECOD), by = TRTA, id = USUBJID,
      denominator = dplyr::rename(ADSL, TRTA = TRT01A))
    Message
      * Removing 1 row from `data` with NA or NaN values in "TRTA", "AESOC", and "AEDECOD" columns.

# ard_stack_hierarchical(denominator) messaging

    Code
      ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD), by = TRTA,
      id = USUBJID, denominator = character())
    Condition
      Error in `ard_stack_hierarchical()`:
      ! The `denominator` argument must be a <data.frame> or an <integer>, not an empty character vector.

---

    Code
      ard_stack_hierarchical(ADAE, variables = c(AESOC, AEDECOD), by = TRTA, id = USUBJID)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! The `denominator` argument cannot be missing.

# ard_stack_hierarchical(variables, include) messaging

    Code
      ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD), include = AESOC,
      by = TRTA, denominator = dplyr::rename(ADSL, TRTA = ARM), id = USUBJID)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! The last column specified in the `variables` (i.e. "AEDECOD") must be in the `include` argument.

# ard_stack_hierarchical(by, overall) messaging

    Code
      ard <- ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD),
      denominator = dplyr::rename(ADSL, TRTA = ARM), id = USUBJID, overall = TRUE)
    Message
      The `by` argument must be specified when using `overall=TRUE`.
      i Setting `overall=FALSE`.

# ard_stack_hierarchical_count(denominator) messaging

    Code
      ard_stack_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD), by = TRTA,
      denominator = letters)
    Condition
      Error in `ard_stack_hierarchical_count()`:
      ! The `denominator` argument must be empty, a <data.frame>, or an <integer>, not a character vector.

# ard_stack_hierarchical_count(denominator,total_n) messaging

    Code
      ard <- ard_stack_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD),
      total_n = TRUE)
    Message
      The `denominator` argument must be specified when using `total_n=TRUE`.
      i Setting `total_n=FALSE`.

# ard_stack_hierarchical_count(overall, denominator) messaging

    Code
      ard <- ard_stack_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD),
      by = TRTA, overall = TRUE)
    Message
      The `denominator` argument must be specified with a data frame when using `overall=TRUE`.
      i Setting `overall=FALSE`.

