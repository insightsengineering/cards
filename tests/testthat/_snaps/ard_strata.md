# ard_strata() works

    Code
      ard_strata(ADSL, .by = ARM, .f = ~ ard_summary(.x, variables = AGE))
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
      i 4 more variables: context, fmt_fun, warning, error

---

    Code
      ard_strata(ADSL, .strata = ARM, .f = ~ ard_summary(.x, variables = AGE, by = AGEGR1))
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
      i 4 more variables: context, fmt_fun, warning, error

# ard_strata computes stats for parameter specific strata

    Code
      as.data.frame(tbl)
    Output
          group1 group1_level variable variable_level  context stat_name stat_label      stat
      1  PARAMCD       PARAM1    AVALC              1 tabulate         n          n         4
      2  PARAMCD       PARAM1    AVALC              1 tabulate         N          N         6
      3  PARAMCD       PARAM1    AVALC              1 tabulate         p          % 0.6666667
      4  PARAMCD       PARAM1    AVALC              2 tabulate         n          n         2
      5  PARAMCD       PARAM1    AVALC              2 tabulate         N          N         6
      6  PARAMCD       PARAM1    AVALC              2 tabulate         p          % 0.3333333
      7  PARAMCD       PARAM2    AVALC              1 tabulate         n          n         0
      8  PARAMCD       PARAM2    AVALC              1 tabulate         N          N         6
      9  PARAMCD       PARAM2    AVALC              1 tabulate         p          %         0
      10 PARAMCD       PARAM2    AVALC              2 tabulate         n          n         3
      11 PARAMCD       PARAM2    AVALC              2 tabulate         N          N         6
      12 PARAMCD       PARAM2    AVALC              2 tabulate         p          %       0.5
      13 PARAMCD       PARAM2    AVALC              3 tabulate         n          n         2
      14 PARAMCD       PARAM2    AVALC              3 tabulate         N          N         6
      15 PARAMCD       PARAM2    AVALC              3 tabulate         p          % 0.3333333
      16 PARAMCD       PARAM2    AVALC              4 tabulate         n          n         1
      17 PARAMCD       PARAM2    AVALC              4 tabulate         N          N         6
      18 PARAMCD       PARAM2    AVALC              4 tabulate         p          % 0.1666667
                                                                                                                                                                                                                                                                                                                           fmt_fun
      1                                                                                                                                                                                                                                                                                                                          0
      2                                                                                                                                                                                                                                                                                                                          0
      3  function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      4                                                                                                                                                                                                                                                                                                                          0
      5                                                                                                                                                                                                                                                                                                                          0
      6  function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      7                                                                                                                                                                                                                                                                                                                          0
      8                                                                                                                                                                                                                                                                                                                          0
      9  function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      10                                                                                                                                                                                                                                                                                                                         0
      11                                                                                                                                                                                                                                                                                                                         0
      12 function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      13                                                                                                                                                                                                                                                                                                                         0
      14                                                                                                                                                                                                                                                                                                                         0
      15 function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      16                                                                                                                                                                                                                                                                                                                         0
      17                                                                                                                                                                                                                                                                                                                         0
      18 function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
         warning error
      1     NULL  NULL
      2     NULL  NULL
      3     NULL  NULL
      4     NULL  NULL
      5     NULL  NULL
      6     NULL  NULL
      7     NULL  NULL
      8     NULL  NULL
      9     NULL  NULL
      10    NULL  NULL
      11    NULL  NULL
      12    NULL  NULL
      13    NULL  NULL
      14    NULL  NULL
      15    NULL  NULL
      16    NULL  NULL
      17    NULL  NULL
      18    NULL  NULL

