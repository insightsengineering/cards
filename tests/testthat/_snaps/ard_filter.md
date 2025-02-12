# ard_filter() works

    Code
      ard_f
    Message
      {cards} data frame: 84 x 15
    Output
         group1 group1_level group2 group2_level group3 group3_level variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo    SEX            F   <NA>                  RACE      BLACK OR…         n          n     3
      2    TRTA      Placebo    SEX            F   <NA>                  RACE      BLACK OR…         N          N     5
      3    TRTA      Placebo    SEX            F   <NA>                  RACE      BLACK OR…         p          %   0.6
      4    TRTA    Xanomeli…    SEX            F   <NA>                  RACE      BLACK OR…         n          n     4
      5    TRTA    Xanomeli…    SEX            F   <NA>                  RACE      BLACK OR…         N          N     6
      6    TRTA    Xanomeli…    SEX            F   <NA>                  RACE      BLACK OR…         p          % 0.667
      7    TRTA    Xanomeli…    SEX            F   <NA>                  RACE      BLACK OR…         n          n     3
      8    TRTA    Xanomeli…    SEX            F   <NA>                  RACE      BLACK OR…         N          N     6
      9    TRTA    Xanomeli…    SEX            F   <NA>                  RACE      BLACK OR…         p          %   0.5
      10   TRTA      Placebo    SEX            F   <NA>                  RACE          WHITE         n          n    10
    Message
      i 74 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fn, warning, error

# ard_filter() error messaging works

    Code
      ard_filter(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"), n > 10)
    Condition
      Error in `ard_filter()`:
      ! Filtering is only available for stacked hierarchical ARDs created using `ard_stack_hierarchical()`.

---

    Code
      ard_filter(ard, 10)
    Condition
      Error in `ard_filter()`:
      ! `filter` must be an expression.

---

    Code
      ard_filter(ard, A > 5)
    Condition
      Error in `ard_filter()`:
      ! The expression provided as `filter` includes condition for statistic or `by` variable "A" which is not present in the ARD.

