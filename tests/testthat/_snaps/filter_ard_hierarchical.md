# filter_ard_hierarchical() works

    Code
      ard_f
    Message
      {cards} data frame: 39 x 15
    Output
         group1 group1_level group2 group2_level group3 group3_level                     variable variable_level stat_name stat_label  stat
      1    <NA>                <NA>                <NA>                                      TRTA        Placebo         n          n    86
      2    <NA>                <NA>                <NA>                                      TRTA        Placebo         N          N   254
      3    <NA>                <NA>                <NA>                                      TRTA        Placebo         p          % 0.339
      4    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…         n          n    84
      5    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…         N          N   254
      6    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…         p          % 0.331
      7    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…         n          n    84
      8    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…         N          N   254
      9    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…         p          % 0.331
      10   TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE         n          n    26
    Message
      i 29 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fun, warning, error

# filter_ard_hierarchical() error messaging works

    Code
      filter_ard_hierarchical(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"),
      n > 10)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! Filtering is only available for stacked hierarchical ARDs created using `ard_stack_hierarchical()` or `ard_stack_hierarchical_count()`.

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

