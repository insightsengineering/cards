# filter_ard_hierarchical() works

    Code
      ard_f
    Message
      {cards} data frame: 84 x 15
    Output
         group1 group1_level group2 group2_level group3 group3_level                     variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo   <NA>                <NA>                                       SEX              F         n          n    13
      2    TRTA      Placebo   <NA>                <NA>                                       SEX              F         N          N    53
      3    TRTA      Placebo   <NA>                <NA>                                       SEX              F         p          % 0.245
      4    TRTA      Placebo   <NA>                <NA>                                       SEX              M         n          n    13
      5    TRTA      Placebo   <NA>                <NA>                                       SEX              M         N          N    33
      6    TRTA      Placebo   <NA>                <NA>                                       SEX              M         p          % 0.394
      7    TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         n          n    26
      8    TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         N          N    86
      9    TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         p          % 0.302
      10   TRTA      Placebo    SEX            F   <NA>                                      RACE      BLACK OR…         n          n     3
    Message
      i 74 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fn, warning, error

# filter_ard_hierarchical() error messaging works

    Code
      filter_ard_hierarchical(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
      n > 10)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! Filtering is only available for stacked hierarchical ARDs created using `ard_stack_hierarchical()`.

---

    Code
      filter_ard_hierarchical(ard, 10)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! `filter` must be an expression.

---

    Code
      filter_ard_hierarchical(ard, A > 5)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! The expression provided as `filter` includes condition for statistic or `by` variable "A" which is not present in the ARD.

