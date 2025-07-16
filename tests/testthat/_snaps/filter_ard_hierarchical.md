# filter_ard_hierarchical() works

    Code
      ard_f
    Message
    
      -- cards -----------------------------------------------------------------------
    Output
         group1 group1_level group2 group2_level group3 group3_level variable                     variable_level stat_name stat_label stat 
      1  <NA>                <NA>                <NA>                TRTA                         Placebo        n         n          86   
      2  <NA>                <NA>                <NA>                TRTA                         Placebo        N         N          254  
      3  <NA>                <NA>                <NA>                TRTA                         Placebo        p         %          0.339
      4  <NA>                <NA>                <NA>                TRTA                         Xanomeli…      n         n          84   
      5  <NA>                <NA>                <NA>                TRTA                         Xanomeli…      N         N          254  
      6  <NA>                <NA>                <NA>                TRTA                         Xanomeli…      p         %          0.331
      7  <NA>                <NA>                <NA>                TRTA                         Xanomeli…      n         n          84   
      8  <NA>                <NA>                <NA>                TRTA                         Xanomeli…      N         N          254  
      9  <NA>                <NA>                <NA>                TRTA                         Xanomeli…      p         %          0.331
      10 TRTA   Placebo      <NA>                <NA>                ..ard_hierarchical_overall.. TRUE           n         n          26   
    Message
      i Showing 10 of 39 rows.

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
      ! The expression provided as `filter` includes condition for statistic "A" which is not present in the ARD and does not correspond to any of the `by` variable levels.
      i Valid filter terms for variable "AETERM" are: "n", "N", "p", "n_1", "n_2", "n_3", "N_1", "N_2", "N_3", "p_1", "p_2", "p_3", "n_overall", "N_overall", "p_overall", and "TRTA".

---

    Code
      filter_ard_hierarchical(ard, n > 1, var = "A")
    Condition
      Error in `filter_ard_hierarchical()`:
      ! Error processing `var` argument.
      ! Can't select columns that don't exist. x Column `A` doesn't exist.
      i Select among columns "SEX", "RACE", and "AETERM"

---

    Code
      filter_ard_hierarchical(ard, n > 1, var = c(SEX, RACE))
    Condition
      Error in `filter_ard_hierarchical()`:
      ! Only one variable can be selected as `var`.

---

    Code
      filter_ard_hierarchical(ard, n > 1, var = RACE)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! No statistics available in the ARD for variable "RACE". In order to filter on "RACE" it must be specified in the `include` argument when the ARD is created.

---

    Code
      filter_ard_hierarchical(ard, n > 1, keep_empty = NULL)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! The `keep_empty` argument must be a scalar with class <logical>, not NULL.

---

    Code
      filter_ard_hierarchical(ard_stat_miss, n_1 > 1)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! The expression provided as `filter` includes condition for statistic "n_1" which is not present in the ARD and does not correspond to any of the `by` variable levels.
      i Valid filter terms for variable "AETERM" are: "p", "p_1", "p_2", "p_3", and "TRTA".

---

    Code
      filter_ard_hierarchical(ard_stat_miss, p_overall > 0.1)
    Condition
      Error in `filter_ard_hierarchical()`:
      ! The expression provided as `filter` includes condition for statistic "p_overall" which is not present in the ARD and does not correspond to any of the `by` variable levels.
      i Valid filter terms for variable "AETERM" are: "p", "p_1", "p_2", "p_3", and "TRTA".

