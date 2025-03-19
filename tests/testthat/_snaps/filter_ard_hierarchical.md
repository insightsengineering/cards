# filter_ard_hierarchical() works

    Code
      ard_f
    Message
      {cards} data frame: 39 x 15
    Output
         group1 group1_level group2 group2_level group3 group3_level                     variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo   <NA>                <NA>                                       SEX              M         n          n    13
      2    TRTA      Placebo   <NA>                <NA>                                       SEX              M         N          N    33
      3    TRTA      Placebo   <NA>                <NA>                                       SEX              M         p          % 0.394
      4    TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         n          n    26
      5    TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         N          N    86
      6    TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         p          % 0.302
      7    TRTA      Placebo    SEX            M   <NA>                                      RACE          WHITE         n          n    12
      8    TRTA      Placebo    SEX            M   <NA>                                      RACE          WHITE         N          N    30
      9    TRTA      Placebo    SEX            M   <NA>                                      RACE          WHITE         p          %   0.4
      10   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              M         n          n    24
    Message
      i 29 more rows
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
      ! The `filter` argument must be an expression.

---

    Code
      filter_ard_hierarchical(ard, A > 5)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! The expression provided as `filter` includes condition for statistic or `by` variable "A" which is not present in the ARD.

---

    Code
      filter_ard_hierarchical(ard, n > 1, keep_empty = NULL)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! The `keep_empty` argument must be a scalar with class <logical>, not NULL.

