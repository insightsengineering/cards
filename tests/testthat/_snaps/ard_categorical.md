# ard_continuous(fmt_fn) argument works

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_categorical(mtcars,
        variables = "am", fmt_fn = list(am = list(p = function(x) as.character(round(
          x * 100, digits = 3)), N = function(x) format(round(x, digits = 2), nsmall = 2),
        N_obs = function(x) format(round(x, digits = 2), nsmall = 2))))), variable,
      variable_level, stat_name, statistic, statistic_fmt))
    Output
         variable variable_level stat_name statistic statistic_fmt
      1        am              0         n        19            19
      2        am              0         p   0.59375        59.375
      3        am              1         n        13            13
      4        am              1         p   0.40625        40.625
      5        am           NULL         N        32         32.00
      6        am           NULL     N_obs        32         32.00
      7        am           NULL    N_miss         0             0
      8        am           NULL N_nonmiss        32            32
      9        am           NULL    p_miss         0             0
      10       am           NULL p_nonmiss         1             1

