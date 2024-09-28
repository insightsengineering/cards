# ard_strata() works

    Code
      ard_strata(ADSL, .by = ARM, .f = ~ ard_continuous(.x, variables = AGE))
    Message
      {cards} data frame: 24 x 10
    Output
         group1 group1_level variable stat_name stat_label   stat
      1     ARM      Placebo      AGE         N          N     86
      2     ARM      Placebo      AGE      mean       Mean 75.209
      3     ARM      Placebo      AGE        sd         SD   8.59
      4     ARM      Placebo      AGE    median     Median     76
      5     ARM      Placebo      AGE       p25         Q1     69
      6     ARM      Placebo      AGE       p75         Q3     82
      7     ARM      Placebo      AGE       min        Min     52
      8     ARM      Placebo      AGE       max        Max     89
      9     ARM    Xanomeli…      AGE         N          N     84
      10    ARM    Xanomeli…      AGE      mean       Mean 74.381
    Message
      i 14 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fn, warning, error

---

    Code
      ard_strata(ADSL, .strata = ARM, .f = ~ ard_continuous(.x, variables = AGE, by = AGEGR1))
    Message
      {cards} data frame: 72 x 12
    Output
         group2 group2_level group1 group1_level variable stat_name stat_label   stat
      1     ARM      Placebo AGEGR1        65-80      AGE         N          N     42
      2     ARM      Placebo AGEGR1        65-80      AGE      mean       Mean 73.595
      3     ARM      Placebo AGEGR1        65-80      AGE        sd         SD  4.173
      4     ARM      Placebo AGEGR1        65-80      AGE    median     Median     74
      5     ARM      Placebo AGEGR1        65-80      AGE       p25         Q1     70
      6     ARM      Placebo AGEGR1        65-80      AGE       p75         Q3     77
      7     ARM      Placebo AGEGR1        65-80      AGE       min        Min     65
      8     ARM      Placebo AGEGR1        65-80      AGE       max        Max     80
      9     ARM      Placebo AGEGR1          <65      AGE         N          N     14
      10    ARM      Placebo AGEGR1          <65      AGE      mean       Mean 61.143
    Message
      i 62 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fn, warning, error

