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
      as.data.frame(tbl)[1:25, ]
    Output
          group2 group2_level group1 group1_level variable variable_level  context
      1  PARAMCD       PARAM1  TREAT      PLACEBO    AVALC              1 tabulate
      2  PARAMCD       PARAM1  TREAT      PLACEBO    AVALC              1 tabulate
      3  PARAMCD       PARAM1  TREAT      PLACEBO    AVALC              1 tabulate
      4  PARAMCD       PARAM1  TREAT      PLACEBO    AVALC              2 tabulate
      5  PARAMCD       PARAM1  TREAT      PLACEBO    AVALC              2 tabulate
      6  PARAMCD       PARAM1  TREAT      PLACEBO    AVALC              2 tabulate
      7  PARAMCD       PARAM1  TREAT        TREAT    AVALC              1 tabulate
      8  PARAMCD       PARAM1  TREAT        TREAT    AVALC              1 tabulate
      9  PARAMCD       PARAM1  TREAT        TREAT    AVALC              1 tabulate
      10 PARAMCD       PARAM1  TREAT        TREAT    AVALC              2 tabulate
      11 PARAMCD       PARAM1  TREAT        TREAT    AVALC              2 tabulate
      12 PARAMCD       PARAM1  TREAT        TREAT    AVALC              2 tabulate
      13 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              1 tabulate
      14 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              1 tabulate
      15 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              1 tabulate
      16 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              2 tabulate
      17 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              2 tabulate
      18 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              2 tabulate
      19 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              3 tabulate
      20 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              3 tabulate
      21 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              3 tabulate
      22 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              4 tabulate
      23 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              4 tabulate
      24 PARAMCD       PARAM2  TREAT      PLACEBO    AVALC              4 tabulate
      25 PARAMCD       PARAM2  TREAT        TREAT    AVALC              1 tabulate
         stat_name stat_label      stat
      1          n          n         1
      2          N          N         3
      3          p          % 0.3333333
      4          n          n         2
      5          N          N         3
      6          p          % 0.6666667
      7          n          n         3
      8          N          N         3
      9          p          %         1
      10         n          n         0
      11         N          N         3
      12         p          %         0
      13         n          n         0
      14         N          N         0
      15         p          %       NaN
      16         n          n         0
      17         N          N         0
      18         p          %       NaN
      19         n          n         0
      20         N          N         0
      21         p          %       NaN
      22         n          n         0
      23         N          N         0
      24         p          %       NaN
      25         n          n         0
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
      19                                                                                                                                                                                                                                                                                                                         0
      20                                                                                                                                                                                                                                                                                                                         0
      21 function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      22                                                                                                                                                                                                                                                                                                                         0
      23                                                                                                                                                                                                                                                                                                                         0
      24 function (x) , {,     res <- ifelse(is.na(x), NA_character_, str_trim(format(round_fun(x * ,         scale, digits = digits), nsmall = digits))),     if (!is.null(width)) {,         res <- ifelse(nchar(res) >= width | is.na(res), res, ,             paste0(strrep(" ", width - nchar(res)), res)),     },     res, }
      25                                                                                                                                                                                                                                                                                                                         0
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
      19    NULL  NULL
      20    NULL  NULL
      21    NULL  NULL
      22    NULL  NULL
      23    NULL  NULL
      24    NULL  NULL
      25    NULL  NULL

