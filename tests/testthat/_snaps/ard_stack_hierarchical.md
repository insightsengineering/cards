# ard_stack_hierarchical(variables) messaging removed obs

    Code
      ard <- ard_stack_hierarchical(dplyr::mutate(ADAE_small, AESOC = ifelse(dplyr::row_number() ==
        1L, NA, AESOC)), variables = c(AESOC, AEDECOD), id = USUBJID, denominator = ADSL)
    Message
      * Removing 1 row from `data` with NA or NaN values in "AESOC" and "AEDECOD" columns.

---

    Code
      ard <- ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD), id = USUBJID,
      by = TRTA, denominator = dplyr::mutate(ADSL, TRTA = ifelse(dplyr::row_number() ==
        1L, NA, TRTA)))
    Message
      * Removing 1 row from `denominator` with NA or NaN values in "TRTA" column.

# ard_stack_hierarchical(variables) messaging

    Code
      ard_stack_hierarchical(ADAE_small, variables = starts_with("xxxxx"), id = USUBJID,
      denominator = ADSL)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! Arguments `variables` and `include` cannot be empty.

---

    Code
      ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD), id = starts_with(
        "xxxxx"), denominator = ADSL)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! Argument `id` cannot be empty.

# ard_stack_hierarchical(by) messaging

    Code
      ard <- ard_stack_hierarchical(dplyr::mutate(ADAE_small, TRTA = ifelse(dplyr::row_number() ==
        1L, NA, TRTA)), variables = c(AESOC, AEDECOD), by = TRTA, id = USUBJID,
      denominator = ADSL)
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
      by = TRTA, denominator = ADSL, id = USUBJID)
    Condition
      Error in `ard_stack_hierarchical()`:
      ! The last column specified in the `variables` (i.e. "AEDECOD") must be in the `include` argument.

# ard_stack_hierarchical(by, overall) messaging

    Code
      ard <- ard_stack_hierarchical(ADAE_small, variables = c(AESOC, AEDECOD),
      denominator = ADSL, id = USUBJID, overall = TRUE)
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
      The `denominator` argument must be specified as a data frame when using `overall=TRUE`.
      i Setting `overall=FALSE`.

# ard_stack_hierarchical_count(overall)

    Code
      dplyr::filter(ard_stack_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD), by = c(TRTA, AESEV), denominator = ADSL, overall = TRUE), !group1 %in% "TRTA" & !group2 %in% "TRTA" & !group3 %in% "TRTA" & !variable %in% "TRTA")
    Message
      {cards} data frame: 18 x 15
    Output
         group1 group1_level group2 group2_level group3 group3_level variable variable_level stat_name stat_label stat
      1   AESEV         MILD   <NA>                <NA>                 AESOC      GENERAL …         n          n    4
      2   AESEV     MODERATE   <NA>                <NA>                 AESOC      GENERAL …         n          n    0
      3    <NA>                <NA>                <NA>                 AESOC      GENERAL …         n          n    4
      4   AESEV         MILD  AESOC    GENERAL …   <NA>               AEDECOD      APPLICAT…         n          n    2
      5   AESEV     MODERATE  AESOC    GENERAL …   <NA>               AEDECOD      APPLICAT…         n          n    0
      6   AESOC    GENERAL …   <NA>                <NA>               AEDECOD      APPLICAT…         n          n    2
      7   AESEV         MILD  AESOC    GENERAL …   <NA>               AEDECOD      APPLICAT…         n          n    2
      8   AESEV     MODERATE  AESOC    GENERAL …   <NA>               AEDECOD      APPLICAT…         n          n    0
      9   AESOC    GENERAL …   <NA>                <NA>               AEDECOD      APPLICAT…         n          n    2
      10  AESEV         MILD   <NA>                <NA>                 AESOC      SKIN AND…         n          n    1
      11  AESEV     MODERATE   <NA>                <NA>                 AESOC      SKIN AND…         n          n    1
      12   <NA>                <NA>                <NA>                 AESOC      SKIN AND…         n          n    2
      13  AESEV         MILD  AESOC    SKIN AND…   <NA>               AEDECOD       ERYTHEMA         n          n    1
      14  AESEV     MODERATE  AESOC    SKIN AND…   <NA>               AEDECOD       ERYTHEMA         n          n    0
      15  AESOC    SKIN AND…   <NA>                <NA>               AEDECOD       ERYTHEMA         n          n    1
      16  AESEV         MILD  AESOC    SKIN AND…   <NA>               AEDECOD      PRURITUS…         n          n    0
      17  AESEV     MODERATE  AESOC    SKIN AND…   <NA>               AEDECOD      PRURITUS…         n          n    1
      18  AESOC    SKIN AND…   <NA>                <NA>               AEDECOD      PRURITUS…         n          n    1
    Message
      i 4 more variables: context, fmt_fun, warning, error

# ard_stack_hierarchical_count(overall,over_variables)

    Code
      as.data.frame(dplyr::select(dplyr::filter(ard_stack_hierarchical_count(ADAE_small, variables = AESOC, by = TRTA, denominator = ADSL, over_variables = TRUE, overall = TRUE), variable == "..ard_hierarchical_overall.."), all_ard_groups(), "variable",
      "stat_name", "stat"))
    Output
        group1         group1_level                     variable stat_name stat
      1   TRTA              Placebo ..ard_hierarchical_overall..         n    2
      2   TRTA Xanomeline High Dose ..ard_hierarchical_overall..         n    2
      3   TRTA  Xanomeline Low Dose ..ard_hierarchical_overall..         n    2
      4   <NA>                 NULL ..ard_hierarchical_overall..         n    6

---

    Code
      dplyr::filter(ard_stack_hierarchical_count(ADAE_small, variables = c(AESOC, AEDECOD), by = c(TRTA, AESEV), denominator = ADSL, overall = TRUE, over_variables = TRUE), !group1 %in% "TRTA" & !group2 %in% "TRTA" & !group3 %in% "TRTA" & !variable %in%
        "TRTA")
    Message
      {cards} data frame: 21 x 15
    Output
         group1 group1_level group2 group2_level group3 group3_level                     variable variable_level stat_name stat_label stat
      1   AESEV         MILD   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         n          n    5
      2   AESEV     MODERATE   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         n          n    1
      3    <NA>                <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         n          n    6
      4   AESEV         MILD   <NA>                <NA>                                     AESOC      GENERAL …         n          n    4
      5   AESEV     MODERATE   <NA>                <NA>                                     AESOC      GENERAL …         n          n    0
      6    <NA>                <NA>                <NA>                                     AESOC      GENERAL …         n          n    4
      7   AESEV         MILD  AESOC    GENERAL …   <NA>                                   AEDECOD      APPLICAT…         n          n    2
      8   AESEV     MODERATE  AESOC    GENERAL …   <NA>                                   AEDECOD      APPLICAT…         n          n    0
      9   AESOC    GENERAL …   <NA>                <NA>                                   AEDECOD      APPLICAT…         n          n    2
      10  AESEV         MILD  AESOC    GENERAL …   <NA>                                   AEDECOD      APPLICAT…         n          n    2
    Message
      i 11 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fun, warning, error

