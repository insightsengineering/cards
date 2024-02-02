# print.card() works

    Code
      ard_continuous(ADSL, by = "ARM", variables = "AGE")
    Message
      {cards} data frame: 24 x 10
    Output
         group1 group1_level variable stat_name stat_label statistic
      1     ARM      Placebo      AGE         N          N        86
      2     ARM      Placebo      AGE      mean       Mean    75.209
      3     ARM      Placebo      AGE        sd         SD      8.59
      4     ARM      Placebo      AGE    median     Median        76
      5     ARM      Placebo      AGE       p25  25th Per…        69
      6     ARM      Placebo      AGE       p75  75th Per…        82
      7     ARM      Placebo      AGE       min        Min        52
      8     ARM      Placebo      AGE       max        Max        89
      9     ARM    Xanomeli…      AGE         N          N        84
      10    ARM    Xanomeli…      AGE      mean       Mean    74.381
    Message
      i 14 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, statistic_fmt_fn, warning, error

---

    Code
      ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
    Message
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level stat_name stat_label statistic
      1     ARM      Placebo   AGEGR1          65-80         n          n        42
      2     ARM      Placebo   AGEGR1          65-80         N          N        86
      3     ARM      Placebo   AGEGR1          65-80         p          %     0.488
      4     ARM    Xanomeli…   AGEGR1          65-80         n          n        55
      5     ARM    Xanomeli…   AGEGR1          65-80         N          N        84
      6     ARM    Xanomeli…   AGEGR1          65-80         p          %     0.655
      7     ARM    Xanomeli…   AGEGR1          65-80         n          n        47
      8     ARM    Xanomeli…   AGEGR1          65-80         N          N        84
      9     ARM    Xanomeli…   AGEGR1          65-80         p          %      0.56
      10    ARM      Placebo   AGEGR1            <65         n          n        14
    Message
      i 17 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, statistic_fmt_fn, warning, error

---

    Code
      ard_continuous(ADSL, variables = "AGE", fmt_fn = AGE ~ list(~ function(x) round(
        x, 3)))
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context stat_name stat_label statistic statistic_fmt_fn
      1      AGE continuo…         N          N       254             <fn>
      2      AGE continuo…      mean       Mean    75.087             <fn>
      3      AGE continuo…        sd         SD     8.246             <fn>
      4      AGE continuo…    median     Median        77             <fn>
      5      AGE continuo…       p25  25th Per…        70             <fn>
      6      AGE continuo…       p75  75th Per…        81             <fn>
      7      AGE continuo…       min        Min        51             <fn>
      8      AGE continuo…       max        Max        89             <fn>
    Message
      i 2 more variables: warning, error

