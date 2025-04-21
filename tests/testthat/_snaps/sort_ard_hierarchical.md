# sort_ard_hierarchical() works

    Code
      print(dplyr::select(ard_s, all_ard_groups(), all_ard_variables()), n = 50)
    Message
      {cards} data frame: 234 x 8
    Output
         group1 group1_level group2 group2_level group3 group3_level                     variable variable_level
      1    <NA>                <NA>                <NA>                                      TRTA        Placebo
      2    <NA>                <NA>                <NA>                                      TRTA        Placebo
      3    <NA>                <NA>                <NA>                                      TRTA        Placebo
      4    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…
      5    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…
      6    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…
      7    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…
      8    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…
      9    <NA>                <NA>                <NA>                                      TRTA      Xanomeli…
      10   TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      11   TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      12   TRTA      Placebo   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      13   TRTA    Xanomeli…   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      14   TRTA    Xanomeli…   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      15   TRTA    Xanomeli…   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      16   TRTA    Xanomeli…   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      17   TRTA    Xanomeli…   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      18   TRTA    Xanomeli…   <NA>                <NA>              ..ard_hierarchical_overall..           TRUE
      19   TRTA      Placebo   <NA>                <NA>                                       SEX              F
      20   TRTA      Placebo   <NA>                <NA>                                       SEX              F
      21   TRTA      Placebo   <NA>                <NA>                                       SEX              F
      22   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              F
      23   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              F
      24   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              F
      25   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              F
      26   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              F
      27   TRTA    Xanomeli…   <NA>                <NA>                                       SEX              F
      28   TRTA      Placebo    SEX            F   <NA>                                      RACE          WHITE
      29   TRTA      Placebo    SEX            F   <NA>                                      RACE          WHITE
      30   TRTA      Placebo    SEX            F   <NA>                                      RACE          WHITE
      31   TRTA    Xanomeli…    SEX            F   <NA>                                      RACE          WHITE
      32   TRTA    Xanomeli…    SEX            F   <NA>                                      RACE          WHITE
      33   TRTA    Xanomeli…    SEX            F   <NA>                                      RACE          WHITE
      34   TRTA    Xanomeli…    SEX            F   <NA>                                      RACE          WHITE
      35   TRTA    Xanomeli…    SEX            F   <NA>                                      RACE          WHITE
      36   TRTA    Xanomeli…    SEX            F   <NA>                                      RACE          WHITE
      37   TRTA      Placebo    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      38   TRTA      Placebo    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      39   TRTA      Placebo    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      40   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      41   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      42   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      43   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      44   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      45   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM      APPLICAT…
      46   TRTA      Placebo    SEX            F   RACE        WHITE                       AETERM       ERYTHEMA
      47   TRTA      Placebo    SEX            F   RACE        WHITE                       AETERM       ERYTHEMA
      48   TRTA      Placebo    SEX            F   RACE        WHITE                       AETERM       ERYTHEMA
      49   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM       ERYTHEMA
      50   TRTA    Xanomeli…    SEX            F   RACE        WHITE                       AETERM       ERYTHEMA
    Message
      i 184 more rows
      i Use `print(n = ...)` to see more rows

# sort_ard_hierarchical() error messaging works

    Code
      sort_ard_hierarchical(ard_categorical(ADSL, by = "ARM", variables = "AGEGR1"))
    Condition
      Error in `sort_ard_hierarchical()`:
      ! Sorting is only available for stacked hierarchical ARDs created using `ard_stack_hierarchical()` or `ard_stack_hierarchical_count()`.

---

    Code
      sort_ard_hierarchical(ard, sort = "no_sorting")
    Condition
      Error in `sort_ard_hierarchical()`:
      ! `sort` must be one of "descending" or "alphanumeric", not "no_sorting".

---

    Code
      sort_ard_hierarchical(ard)
    Condition
      Error in `sort_ard_hierarchical()`:
      ! If `sort='descending'` then either "n" or "p" must be present in `x` for all variables in order to calculate the count sums used for sorting.

