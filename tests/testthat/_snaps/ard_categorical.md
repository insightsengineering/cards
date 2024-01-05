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
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_categorical(mtcars,
        variables = "am", fmt_fn = list(am = list(p = function(x) as.character(round5(
          x * 100, digits = 3)), N = function(x) format(round5(x, digits = 2),
        nsmall = 2), N_obs = function(x) format(round5(x, digits = 2), nsmall = 2))))),
      variable, variable_level, stat_name, statistic, statistic_fmt))
    Output
        variable variable_level stat_name statistic statistic_fmt
      1       am              0         n        19            19
      2       am              0         N        32         32.00
      3       am              0         p   0.59375        59.375
      4       am              1         n        13            13
      5       am              1         N        32         32.00
      6       am              1         p   0.40625        40.625

---

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_categorical(mtcars,
        variables = c("am", "vs"), fmt_fn = list(am = list(p = function(x) round5(x *
          100, digits = 3)), vs = list(p = function(x) round5(x * 100, digits = 1))))),
      variable, variable_level, stat_name, statistic, statistic_fmt))
    Output
         variable variable_level stat_name statistic statistic_fmt
      1        am              0         n        19            19
      2        am              0         N        32            32
      3        am              0         p   0.59375        59.375
      4        am              1         n        13            13
      5        am              1         N        32            32
      6        am              1         p   0.40625        40.625
      7        vs              0         n        18            18
      8        vs              0         N        32            32
      9        vs              0         p    0.5625          56.3
      10       vs              1         n        14            14
      11       vs              1         N        32            32
      12       vs              1         p    0.4375          43.8

# ard_categorical() with strata and by arguments

    Code
      ard_categorical(ADSL, by = "ARM", variables = "AGEGR1", denominator = dplyr::filter(
        ADSL, ARM %in% "Placebo"))
    Condition
      Error in `ard_categorical()`:
      ! The following `by/strata` combinations are missing from the `denominator` data frame: ARM (Xanomeline High Dose) and ARM (Xanomeline Low Dose).

# ard_categorical(stat_labels) argument works

    Code
      unique(dplyr::select(dplyr::filter(ard_categorical(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_labels = everything() ~ list(c("n", "p") ~
          "n (pct)")), stat_name %in% c("n", "p")), stat_name, stat_label))
    Output
      # A tibble: 2 x 2
        stat_name stat_label
        <chr>     <chr>     
      1 n         n (pct)   
      2 p         n (pct)   

---

    Code
      unique(dplyr::select(dplyr::filter(ard_categorical(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_labels = everything() ~ list(n = "num",
          p = "pct")), stat_name %in% c("n", "p")), stat_name, stat_label))
    Output
      # A tibble: 2 x 2
        stat_name stat_label
        <chr>     <chr>     
      1 n         num       
      2 p         pct       

---

    Code
      unique(dplyr::select(dplyr::filter(ard_categorical(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_labels = AGEGR1 ~ list(c("n", "p") ~
          "n (pct)")), stat_name %in% c("n", "p")), variable, stat_name, stat_label))
    Output
      # A tibble: 4 x 3
        variable stat_name stat_label
        <chr>    <chr>     <chr>     
      1 AGEGR1   n         n (pct)   
      2 AGEGR1   p         n (pct)   
      3 SEX      n         n         
      4 SEX      p         %         

# ard_categorical(denominator='row') works

    Code
      as.data.frame(dplyr::select(flatten_ard(apply_statistic_fmt_fn(ard_with_args)),
      -statistic_fmt_fn, -warning, -error))
    Output
         group1         group1_level variable variable_level stat_name stat_label
      1     ARM              Placebo   AGEGR1          65-80         n          n
      2     ARM              Placebo   AGEGR1          65-80         N          N
      3     ARM              Placebo   AGEGR1            <65         n          n
      4     ARM              Placebo   AGEGR1            <65         N          N
      5     ARM              Placebo   AGEGR1            >80         n          n
      6     ARM              Placebo   AGEGR1            >80         N          N
      7     ARM Xanomeline High Dose   AGEGR1          65-80         n          n
      8     ARM Xanomeline High Dose   AGEGR1          65-80         N          N
      9     ARM Xanomeline High Dose   AGEGR1            <65         n          n
      10    ARM Xanomeline High Dose   AGEGR1            <65         N          N
      11    ARM Xanomeline High Dose   AGEGR1            >80         n          n
      12    ARM Xanomeline High Dose   AGEGR1            >80         N          N
      13    ARM  Xanomeline Low Dose   AGEGR1          65-80         n          n
      14    ARM  Xanomeline Low Dose   AGEGR1          65-80         N          N
      15    ARM  Xanomeline Low Dose   AGEGR1            <65         n          n
      16    ARM  Xanomeline Low Dose   AGEGR1            <65         N          N
      17    ARM  Xanomeline Low Dose   AGEGR1            >80         n          n
      18    ARM  Xanomeline Low Dose   AGEGR1            >80         N          N
         statistic statistic_fmt
      1         42         42.00
      2        144           144
      3         14         14.00
      4         33            33
      5         30         30.00
      6         77            77
      7         55         55.00
      8        144           144
      9         11         11.00
      10        33            33
      11        18         18.00
      12        77            77
      13        47         47.00
      14       144           144
      15         8          8.00
      16        33            33
      17        29         29.00
      18        77            77

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

# ard_categorical(statistics) works with custom fns

    Code
      ard_custom_fns <- flatten_ard(ard_categorical(ADSL, variables = AGEGR1,
        statistics = ~ categorical_variable_summary_fns(other_stats = list(mode = function(
          x) getElement(names(sort(table(x), decreasing = TRUE)), 1), length = function(
          x) length(x)))))

