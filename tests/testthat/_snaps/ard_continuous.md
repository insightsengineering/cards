# ard_continuous() works

    Code
      class(ard_test)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

# ard_continuous(fmt_fn) argument works

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_continuous(ADSL,
        variables = "AGE", statistics = list(AGE = continuous_variable_summary_fns(c(
          "N", "mean", "median"))), fmt_fn = list(AGE = list(mean = function(x)
          as.character(round5(x, digits = 3)), N = function(x) format(round5(x,
          digits = 2), nsmall = 2), N_obs = function(x) format(round5(x, digits = 2),
        nsmall = 2))))), variable, stat_name, statistic, statistic_fmt))
    Output
        variable stat_name statistic statistic_fmt
      1      AGE         N       254        254.00
      2      AGE      mean  75.08661        75.087
      3      AGE    median        77          77.0

---

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_continuous(ADSL,
        variables = c("AGE", "BMIBL"), statistics = ~ continuous_variable_summary_fns(
          "mean"), fmt_fn = list(AGE = list(mean = function(x) as.character(round5(x,
          digits = 3)))))), variable, stat_name, statistic, statistic_fmt))
    Output
        variable stat_name statistic statistic_fmt
      1      AGE      mean  75.08661        75.087
      2    BMIBL      mean  24.67233          24.7

---

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_continuous(ADSL,
        variables = c("AGE", "BMIBL"), statistics = ~ continuous_variable_summary_fns(
          c("mean", "sd")), fmt_fn = ~ list(~ function(x) round(x, 4)))), variable,
      stat_name, statistic, statistic_fmt))
    Output
        variable stat_name statistic statistic_fmt
      1      AGE      mean  75.08661       75.0866
      2      AGE        sd  8.246234        8.2462
      3    BMIBL      mean  24.67233       24.6723
      4    BMIBL        sd  4.092185        4.0922

# ard_continuous() messaging

    Code
      ard_continuous(mtcars, variables = "mpg", statistics = ~ list(mean = "this is a string"))
    Condition
      Error:
      ! Error in the argument `statistics` for variable "mpg".
      i Value must be a named list of functions.

---

    Code
      ard_continuous(letters, variables = "mpg")
    Condition
      Error in `ard_continuous()`:
      ! The `data` argument must be class <data.frame>.

---

    Code
      ard_continuous(mtcars)
    Condition
      Error in `ard_continuous()`:
      ! The `variables` argument cannot be missing.

# ard_continuous(stat_labels) argument works

    Code
      unique(dplyr::filter(dplyr::select(as.data.frame(ard_continuous(data = ADSL,
        by = "ARM", variables = c("AGE", "BMIBL"), stat_labels = everything() ~ list(
          c("min", "max") ~ "min - max"))), stat_name, stat_label), stat_name %in% c(
        "min", "max")))
    Output
        stat_name stat_label
      1       min  min - max
      2       max  min - max

---

    Code
      unique(dplyr::filter(dplyr::select(as.data.frame(ard_continuous(data = ADSL,
        by = "ARM", variables = c("AGE", "BMIBL"), stat_labels = everything() ~ list(
          p25 = "25th %ile", p75 = "75th %ile"))), stat_name, stat_label),
      stat_name %in% c("p25", "p75")))
    Output
        stat_name stat_label
      1       p25  25th %ile
      2       p75  75th %ile

---

    Code
      unique(dplyr::select(dplyr::filter(as.data.frame(ard_continuous(data = ADSL,
        by = "ARM", variables = c("AGE", "BMIBL"), stat_labels = AGE ~ list(p25 = "25th %ile",
          p75 = "75th %ile"))), stat_name %in% c("p25", "p75")), variable, stat_name,
      stat_label))
    Output
        variable stat_name      stat_label
      1      AGE       p25       25th %ile
      2      AGE       p75       75th %ile
      3    BMIBL       p25 25th Percentile
      4    BMIBL       p75 75th Percentile

---

    Code
      ard1
    Output
        variable stat_name stat_label
      1      AGE  conf.low         LB
      2      AGE conf.high         UB

