# ard_stack_hierarchical(variables) messaging

    Code
      ard <- ard_stack_hierarchical(dplyr::mutate(ADAE_small, AESOC = ifelse(dplyr::row_number() ==
        1L, NA, AESOC)), variables = c(AESOC, AEDECOD), id = USUBJID, denominator = dplyr::rename(
        ADSL, TRTA = TRT01A))
    Message
      * Removing 1 row from `data` with NA or NaN values in "AESOC" and "AEDECOD" columns.

---

    Code
      ard_stack_hierarchical(ADAE_small, variables = starts_with("xxxxx"), id = USUBJID,
      denominator = dplyr::rename(ADSL, TRTA = TRT01A))
    Condition
      Error in `ard_stack_hierarchical()`:
      ! Arguments `variables` and `include` cannot be empty.

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
      denominator = letters)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! The `denominator` argument must be empty, a <data.frame>, or an <integer>, not a character vector.

# ard_stack_hierarchical(denominator,total_n) messaging

    Code
      ard <- ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD),
      total_n = TRUE)
    Message
      The `denominator` argument must be specified when using `total_n=TRUE`.
      i Setting `ard_stack_hierarchical(total_n=FALSE)`.

# ard_stack_hierarchical(id,denominator) messaging

    Code
      ard_stack_hierarchical(ADAE, variables = c(AESOC, AEDECOD), by = TRTA, id = USUBJID)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! The `denominator` must be specified when the `id` argument is specified.

