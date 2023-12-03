# ard_continuous(fmt_fn) argument works

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_continuous(ADSL,
        variables = "AGE", statistics = list(AGE = continuous_variable_summary_fns(c(
          "N", "mean", "median"))), fmt_fn = list(AGE = list(mean = function(x)
          as.character(round(x, digits = 3)), N = function(x) format(round(x, digits = 2),
        nsmall = 2), N_obs = function(x) format(round(x, digits = 2), nsmall = 2))))),
      variable, stat_name, statistic, statistic_fmt))
    Output
        variable stat_name statistic statistic_fmt
      1      AGE         N       254        254.00
      2      AGE      mean  75.08661        75.087
      3      AGE    median        77            77
      4      AGE     N_obs       254        254.00
      5      AGE    N_miss         0             0
      6      AGE N_nonmiss       254           254
      7      AGE    p_miss         0             0
      8      AGE p_nonmiss         1             1

# ard_continuous() messaging

    Code
      ard_continuous(mtcars, variables = "mpg", statistics = ~ list(mean = "this is a string"))
    Condition
      Error:
      ! Error in the argument `statistics` for variable "mpg".
      i Value must be a named list of functions.

