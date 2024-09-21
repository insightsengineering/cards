# ard_stack_hierarchical(variables) messaging

    Code
      ard <- ard_stack_hierarchical(dplyr::mutate(ADAE_small, AESOC = ifelse(dplyr::row_number() ==
        1L, NA, AESOC)), variables = c(AESOC, AEDECOD), id = USUBJID, denominator = ADSL)
    Message
      * Removing 1 row from `data` with NA or NaN values in "AESOC" and "AEDECOD" columns.

---

    Code
      ard_stack_hierarchical(ADAE_small, variables = starts_with("xxxxx"), id = USUBJID,
      denominator = ADSL)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! Arguments `variables` and `include` cannot be empty.

