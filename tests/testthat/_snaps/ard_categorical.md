# ard_categorical() univariate

    Code
      class(ard_cat_uni)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_categorical() univariate & specified denomiator

    Code
      class(ard_cat_new_denom)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_continuous(fmt_fn) argument works

    Code
      as.data.frame(dplyr::select(apply_fmt_fn(ard_categorical(mtcars, variables = "am",
        fmt_fn = list(am = list(p = function(x) as.character(round5(x * 100, digits = 3)),
        N = function(x) format(round5(x, digits = 2), nsmall = 2), N_obs = function(x)
          format(round5(x, digits = 2), nsmall = 2))))), variable, variable_level,
      stat_name, stat, stat_fmt))
    Output
        variable variable_level stat_name    stat stat_fmt
      1       am              0         n      19       19
      2       am              0         N      32    32.00
      3       am              0         p 0.59375   59.375
      4       am              1         n      13       13
      5       am              1         N      32    32.00
      6       am              1         p 0.40625   40.625

---

    Code
      as.data.frame(dplyr::select(apply_fmt_fn(ard_categorical(mtcars, variables = c(
        "am", "vs"), fmt_fn = list(am = list(p = function(x) round5(x * 100, digits = 3)),
      vs = list(p = function(x) round5(x * 100, digits = 1))))), variable,
      variable_level, stat_name, stat, stat_fmt))
    Output
         variable variable_level stat_name    stat stat_fmt
      1        am              0         n      19       19
      2        am              0         N      32       32
      3        am              0         p 0.59375   59.375
      4        am              1         n      13       13
      5        am              1         N      32       32
      6        am              1         p 0.40625   40.625
      7        vs              0         n      18       18
      8        vs              0         N      32       32
      9        vs              0         p  0.5625     56.3
      10       vs              1         n      14       14
      11       vs              1         N      32       32
      12       vs              1         p  0.4375     43.8

# ard_categorical() with strata and by arguments

    Code
      ard_categorical(ADSL, by = "ARM", variables = "AGEGR1", denominator = dplyr::filter(
        ADSL, ARM %in% "Placebo"))
    Condition
      Error in `ard_categorical()`:
      ! The following `by/strata` combinations are missing from the `denominator` data frame: ARM (Xanomeline High Dose) and ARM (Xanomeline Low Dose).

# ard_categorical(stat_label) argument works

    Code
      unique(dplyr::select(dplyr::filter(as.data.frame(ard_categorical(data = ADSL,
        by = "ARM", variables = c("AGEGR1", "SEX"), stat_label = everything() ~ list(
          c("n", "p") ~ "n (pct)"))), stat_name %in% c("n", "p")), stat_name,
      stat_label))
    Output
        stat_name stat_label
      1         n    n (pct)
      2         p    n (pct)

---

    Code
      unique(dplyr::select(dplyr::filter(as.data.frame(ard_categorical(data = ADSL,
        by = "ARM", variables = c("AGEGR1", "SEX"), stat_label = everything() ~ list(
          n = "num", p = "pct"))), stat_name %in% c("n", "p")), stat_name, stat_label))
    Output
        stat_name stat_label
      1         n        num
      2         p        pct

---

    Code
      unique(dplyr::select(dplyr::filter(as.data.frame(ard_categorical(data = ADSL,
        by = "ARM", variables = c("AGEGR1", "SEX"), stat_label = AGEGR1 ~ list(c("n",
          "p") ~ "n (pct)"))), stat_name %in% c("n", "p")), variable, stat_name,
      stat_label))
    Output
         variable stat_name stat_label
      1    AGEGR1         n    n (pct)
      2    AGEGR1         p    n (pct)
      19      SEX         n          n
      20      SEX         p          %

# ard_categorical(denominator='row') works

    Code
      as.data.frame(dplyr::select(apply_fmt_fn(ard_with_args), -fmt_fn, -warning,
      -error))
    Output
         group1         group1_level variable variable_level     context stat_name
      1     ARM              Placebo   AGEGR1          65-80 categorical         n
      2     ARM              Placebo   AGEGR1          65-80 categorical         N
      3     ARM Xanomeline High Dose   AGEGR1          65-80 categorical         n
      4     ARM Xanomeline High Dose   AGEGR1          65-80 categorical         N
      5     ARM  Xanomeline Low Dose   AGEGR1          65-80 categorical         n
      6     ARM  Xanomeline Low Dose   AGEGR1          65-80 categorical         N
      7     ARM              Placebo   AGEGR1            <65 categorical         n
      8     ARM              Placebo   AGEGR1            <65 categorical         N
      9     ARM Xanomeline High Dose   AGEGR1            <65 categorical         n
      10    ARM Xanomeline High Dose   AGEGR1            <65 categorical         N
      11    ARM  Xanomeline Low Dose   AGEGR1            <65 categorical         n
      12    ARM  Xanomeline Low Dose   AGEGR1            <65 categorical         N
      13    ARM              Placebo   AGEGR1            >80 categorical         n
      14    ARM              Placebo   AGEGR1            >80 categorical         N
      15    ARM Xanomeline High Dose   AGEGR1            >80 categorical         n
      16    ARM Xanomeline High Dose   AGEGR1            >80 categorical         N
      17    ARM  Xanomeline Low Dose   AGEGR1            >80 categorical         n
      18    ARM  Xanomeline Low Dose   AGEGR1            >80 categorical         N
         stat_label stat stat_fmt
      1           n   42    42.00
      2           N  144      144
      3           n   55    55.00
      4           N  144      144
      5           n   47    47.00
      6           N  144      144
      7           n   14    14.00
      8           N   33       33
      9           n   11    11.00
      10          N   33       33
      11          n    8     8.00
      12          N   33       33
      13          n   30    30.00
      14          N   77       77
      15          n   18    18.00
      16          N   77       77
      17          n   29    29.00
      18          N   77       77

# ard_categorical(denominator=<data frame with counts>) works

    Code
      ard_categorical(ADSL, by = ARM, variables = AGEGR1, denominator = data.frame(
        ARM = c("Placebo", "Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
        ...ard_N... = c(86, 86, 84, 84)))
    Condition
      Error in `ard_categorical()`:
      ! Specified counts in column "'...ard_N...'" are not unique in the `denominator` argument across the `by` and `strata` columns.

---

    Code
      ard_categorical(ADSL, by = ARM, variables = AGEGR1, denominator = data.frame(
        ARM = "Placebo", ...ard_N... = 86))
    Condition
      Error in `ard_categorical()`:
      ! The following `by/strata` combinations are missing from the `denominator` data frame: ARM (Xanomeline High Dose) and ARM (Xanomeline Low Dose).

# ard_categorical() and all NA columns

    Code
      ard_categorical(dplyr::mutate(ADSL, AGEGR1 = NA_character_), variables = AGEGR1)
    Condition
      Error in `ard_categorical()`:
      ! Column "AGEGR1" is all missing and cannot by tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

# ard_categorical(by) messages about protected names

    Code
      ard_categorical(mtcars2, by = variable, variables = gear)
    Condition
      Error in `ard_categorical()`:
      ! The `by` argument cannot include variables named "variable" and "variable_level".

---

    Code
      ard_categorical(mtcars2, by = variable, variables = by)
    Condition
      Error in `ard_categorical()`:
      ! The `by` argument cannot include variables named "variable" and "variable_level".

