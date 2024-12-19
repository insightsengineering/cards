# ARD helpers messaging

    Code
      bind_ard(ard, ard, .update = letters)
    Condition
      Error in `bind_ard()`:
      ! The `.update` argument must be a scalar with class <logical>, not a character vector.

---

    Code
      bind_ard(ard, ard, .distinct = FALSE, .update = FALSE)
    Condition
      Error in `bind_ard()`:
      ! 27 rows with duplicated statistic names have been found.
      i See cards::bind_ard(.update) (`?cards::bind_ard()`) for details.

# bind_ard() .order argument works

    Code
      dplyr::select(as.data.frame(bind_ard(ard_categorical(ADSL, by = "ARM",
        variables = "SEX") %>% {
        dplyr::slice(., sample.int(nrow(.)))
      }, .order = TRUE)), -c(context, fmt_fn, warning, error))
    Output
         group1         group1_level variable variable_level stat_name stat_label
      1     ARM  Xanomeline Low Dose      SEX              M         n          n
      2     ARM  Xanomeline Low Dose      SEX              M         p          %
      3     ARM  Xanomeline Low Dose      SEX              F         p          %
      4     ARM  Xanomeline Low Dose      SEX              M         N          N
      5     ARM  Xanomeline Low Dose      SEX              F         n          n
      6     ARM  Xanomeline Low Dose      SEX              F         N          N
      7     ARM              Placebo      SEX              M         p          %
      8     ARM              Placebo      SEX              M         n          n
      9     ARM              Placebo      SEX              F         n          n
      10    ARM              Placebo      SEX              F         N          N
      11    ARM              Placebo      SEX              F         p          %
      12    ARM              Placebo      SEX              M         N          N
      13    ARM Xanomeline High Dose      SEX              M         N          N
      14    ARM Xanomeline High Dose      SEX              M         p          %
      15    ARM Xanomeline High Dose      SEX              F         p          %
      16    ARM Xanomeline High Dose      SEX              F         N          N
      17    ARM Xanomeline High Dose      SEX              F         n          n
      18    ARM Xanomeline High Dose      SEX              M         n          n
              stat
      1         34
      2  0.4047619
      3  0.5952381
      4         84
      5         50
      6         84
      7  0.3837209
      8         33
      9         53
      10        86
      11 0.6162791
      12        86
      13        84
      14 0.5238095
      15 0.4761905
      16        84
      17        40
      18        44

---

    Code
      dplyr::select(as.data.frame(bind_ard(ard_categorical(ADSL, by = "ARM",
        variables = "SEX") %>% {
        dplyr::slice(., sample.int(nrow(.)))
      }, .order = FALSE)), -c(context, fmt_fn, warning, error))
    Output
         group1         group1_level variable variable_level stat_name stat_label
      1     ARM              Placebo      SEX              F         p          %
      2     ARM  Xanomeline Low Dose      SEX              F         N          N
      3     ARM              Placebo      SEX              M         n          n
      4     ARM Xanomeline High Dose      SEX              F         n          n
      5     ARM Xanomeline High Dose      SEX              F         p          %
      6     ARM              Placebo      SEX              F         n          n
      7     ARM Xanomeline High Dose      SEX              M         n          n
      8     ARM Xanomeline High Dose      SEX              M         N          N
      9     ARM              Placebo      SEX              M         p          %
      10    ARM              Placebo      SEX              M         N          N
      11    ARM  Xanomeline Low Dose      SEX              F         p          %
      12    ARM              Placebo      SEX              F         N          N
      13    ARM  Xanomeline Low Dose      SEX              M         p          %
      14    ARM Xanomeline High Dose      SEX              M         p          %
      15    ARM Xanomeline High Dose      SEX              F         N          N
      16    ARM  Xanomeline Low Dose      SEX              F         n          n
      17    ARM  Xanomeline Low Dose      SEX              M         N          N
      18    ARM  Xanomeline Low Dose      SEX              M         n          n
              stat
      1  0.6162791
      2         84
      3         33
      4         40
      5  0.4761905
      6         53
      7         44
      8         84
      9  0.3837209
      10        86
      11 0.5952381
      12        86
      13 0.4047619
      14 0.5238095
      15        84
      16        50
      17        84
      18        34

# bind_ard(.distinct)

    Code
      ard_continuous(ADSL, variables = AGE) %>% {
        bind_ard(., ., .update = FALSE)
      }
    Message
      i 8 rows with duplicated statistic values have been removed.
      * See cards::bind_ard(.distinct) (`?cards::bind_ard()`) for details.
      {cards} data frame: 8 x 8
    Output
        variable   context stat_name stat_label   stat fmt_fn
      1      AGE continuo…         N          N    254      0
      2      AGE continuo…      mean       Mean 75.087      1
      3      AGE continuo…        sd         SD  8.246      1
      4      AGE continuo…    median     Median     77      1
      5      AGE continuo…       p25         Q1     70      1
      6      AGE continuo…       p75         Q3     81      1
      7      AGE continuo…       min        Min     51      1
      8      AGE continuo…       max        Max     89      1
    Message
      i 2 more variables: warning, error

