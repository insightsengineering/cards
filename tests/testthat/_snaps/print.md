# print.card() works

    Code
      ard_continuous(ADSL, by = "ARM", variables = "AGE")
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
      ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
    Message
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level stat_name stat_label  stat
      1     ARM      Placebo   AGEGR1          65-80         n          n    42
      2     ARM      Placebo   AGEGR1          65-80         N          N    86
      3     ARM      Placebo   AGEGR1          65-80         p          % 0.488
      4     ARM      Placebo   AGEGR1            <65         n          n    14
      5     ARM      Placebo   AGEGR1            <65         N          N    86
      6     ARM      Placebo   AGEGR1            <65         p          % 0.163
      7     ARM      Placebo   AGEGR1            >80         n          n    30
      8     ARM      Placebo   AGEGR1            >80         N          N    86
      9     ARM      Placebo   AGEGR1            >80         p          % 0.349
      10    ARM    Xanomeli…   AGEGR1          65-80         n          n    55
    Message
      i 17 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fun, warning, error

---

    Code
      ard_continuous(ADSL, variables = "AGE", fmt_fun = AGE ~ list(~ function(x)
        round(x, 3)))
    Message
      {cards} data frame: 8 x 8
    Output
        variable   context stat_name stat_label   stat fmt_fun
      1      AGE continuo…         N          N    254    <fn>
      2      AGE continuo…      mean       Mean 75.087    <fn>
      3      AGE continuo…        sd         SD  8.246    <fn>
      4      AGE continuo…    median     Median     77    <fn>
      5      AGE continuo…       p25         Q1     70    <fn>
      6      AGE continuo…       p75         Q3     81    <fn>
      7      AGE continuo…       min        Min     51    <fn>
      8      AGE continuo…       max        Max     89    <fn>
    Message
      i 2 more variables: warning, error

---

    Code
      dplyr::select(ard_continuous(data = data.frame(x = seq(as.Date("2000-01-01"),
      length.out = 10L, by = "day")), variables = x, statistic = ~
      continuous_summary_fns(c("min", "max", "sd"))), -fmt_fun)
    Message
      {cards} data frame: 3 x 7
    Output
        variable   context stat_name stat_label      stat error
      1        x continuo…       min        Min 2000-01-…      
      2        x continuo…       max        Max 2000-01-…      
      3        x continuo…        sd         SD     3.028      
    Message
      i 1 more variable: warning

---

    Code
      bind_ard(ard_attributes(mtcars, variables = mpg), ard_continuous(mtcars,
        variables = mpg, statistic = ~ continuous_summary_fns("mean", other_stats = list(
          vcov = function(x) vcov(lm(mpg ~ am, mtcars))))))
    Message
      {cards} data frame: 4 x 8
    Output
        variable   context stat_name stat_label                         stat fmt_fun
      1      mpg attribut…     label  Variable…                          mpg    <fn>
      2      mpg attribut…     class  Variable…                      numeric    NULL
      3      mpg continuo…      mean       Mean                       20.091       1
      4      mpg continuo…      vcov       vcov 1.265, -1.265, -1.265, 3.113       1
    Message
      i 2 more variables: warning, error

# print.card() n parameter controls row display

    Code
      print.card(card_obj, n = 5)
    Output
      {cards} data frame: 24 x 10
        group1 group1_level variable stat_name stat_label   stat
      1    ARM      Placebo      AGE         N          N     86
      2    ARM      Placebo      AGE      mean       Mean 75.209
      3    ARM      Placebo      AGE        sd         SD   8.59
      4    ARM      Placebo      AGE    median     Median     76
      5    ARM      Placebo      AGE       p25         Q1     69
      # ... with 19 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

---

    Code
      print.card(card_obj, n = 15)
    Output
      {cards} data frame: 24 x 10
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
      11    ARM    Xanomeli…      AGE        sd         SD  7.886
      12    ARM    Xanomeli…      AGE    median     Median     76
      13    ARM    Xanomeli…      AGE       p25         Q1   70.5
      14    ARM    Xanomeli…      AGE       p75         Q3     80
      15    ARM    Xanomeli…      AGE       min        Min     56
      # ... with 9 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

---

    Code
      print.card(card_obj, n = 50)
    Output
      {cards} data frame: 24 x 10
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
      11    ARM    Xanomeli…      AGE        sd         SD  7.886
      12    ARM    Xanomeli…      AGE    median     Median     76
      13    ARM    Xanomeli…      AGE       p25         Q1   70.5
      14    ARM    Xanomeli…      AGE       p75         Q3     80
      15    ARM    Xanomeli…      AGE       min        Min     56
      16    ARM    Xanomeli…      AGE       max        Max     88
      17    ARM    Xanomeli…      AGE         N          N     84
      18    ARM    Xanomeli…      AGE      mean       Mean 75.667
      19    ARM    Xanomeli…      AGE        sd         SD  8.286
      20    ARM    Xanomeli…      AGE    median     Median   77.5
      21    ARM    Xanomeli…      AGE       p25         Q1     71
      22    ARM    Xanomeli…      AGE       p75         Q3     82
      23    ARM    Xanomeli…      AGE       min        Min     51
      24    ARM    Xanomeli…      AGE       max        Max     88
      # ... with 4 more variables: context, fmt_fun, warning, error

# print.card() columns parameter works

    Code
      print.card(card_obj, columns = "auto")
    Output
      {cards} data frame: 24 x 10
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
      # ... with 14 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

---

    Code
      print.card(card_obj, columns = "all")
    Output
      {cards} data frame: 24 x 10
         group1 group1_level variable   context stat_name stat_label   stat fmt_fun
      1     ARM      Placebo      AGE continuo…         N          N     86       0
      2     ARM      Placebo      AGE continuo…      mean       Mean 75.209       1
      3     ARM      Placebo      AGE continuo…        sd         SD   8.59       1
      4     ARM      Placebo      AGE continuo…    median     Median     76       1
      5     ARM      Placebo      AGE continuo…       p25         Q1     69       1
      6     ARM      Placebo      AGE continuo…       p75         Q3     82       1
      7     ARM      Placebo      AGE continuo…       min        Min     52       1
      8     ARM      Placebo      AGE continuo…       max        Max     89       1
      9     ARM    Xanomeli…      AGE continuo…         N          N     84       0
      10    ARM    Xanomeli…      AGE continuo…      mean       Mean 74.381       1
         warning error
      1               
      2               
      3               
      4               
      5               
      6               
      7               
      8               
      9               
      10              
      # ... with 14 more rows

# print.card() n_col parameter controls column threshold

    Code
      print.card(card_obj, n_col = 3)
    Output
      {cards} data frame: 24 x 10
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
      # ... with 14 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

---

    Code
      print.card(card_obj, n_col = 8)
    Output
      {cards} data frame: 24 x 10
         group1 group1_level variable   context stat_name stat_label   stat fmt_fun
      1     ARM      Placebo      AGE continuo…         N          N     86       0
      2     ARM      Placebo      AGE continuo…      mean       Mean 75.209       1
      3     ARM      Placebo      AGE continuo…        sd         SD   8.59       1
      4     ARM      Placebo      AGE continuo…    median     Median     76       1
      5     ARM      Placebo      AGE continuo…       p25         Q1     69       1
      6     ARM      Placebo      AGE continuo…       p75         Q3     82       1
      7     ARM      Placebo      AGE continuo…       min        Min     52       1
      8     ARM      Placebo      AGE continuo…       max        Max     89       1
      9     ARM    Xanomeli…      AGE continuo…         N          N     84       0
      10    ARM    Xanomeli…      AGE continuo…      mean       Mean 74.381       1
      # ... with 14 more rows
      # ... with 2 more variables: warning, error

---

    Code
      print.card(card_obj, n_col = 15)
    Output
      {cards} data frame: 24 x 10
         group1 group1_level variable   context stat_name stat_label   stat fmt_fun
      1     ARM      Placebo      AGE continuo…         N          N     86       0
      2     ARM      Placebo      AGE continuo…      mean       Mean 75.209       1
      3     ARM      Placebo      AGE continuo…        sd         SD   8.59       1
      4     ARM      Placebo      AGE continuo…    median     Median     76       1
      5     ARM      Placebo      AGE continuo…       p25         Q1     69       1
      6     ARM      Placebo      AGE continuo…       p75         Q3     82       1
      7     ARM      Placebo      AGE continuo…       min        Min     52       1
      8     ARM      Placebo      AGE continuo…       max        Max     89       1
      9     ARM    Xanomeli…      AGE continuo…         N          N     84       0
      10    ARM    Xanomeli…      AGE continuo…      mean       Mean 74.381       1
         warning error
      1               
      2               
      3               
      4               
      5               
      6               
      7               
      8               
      9               
      10              
      # ... with 14 more rows

# print.card() handles list columns properly

    Code
      print.card(card_obj)
    Output
      {cards} data frame: 24 x 10
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
      # ... with 14 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

# print.card() output formatting is consistent

    Code
      print.card(continuous_card)
    Output
      {cards} data frame: 8 x 8
        variable   context stat_name stat_label   stat fmt_fun
      1      AGE continuo…         N          N    254       0
      2      AGE continuo…      mean       Mean 75.087       1
      3      AGE continuo…        sd         SD  8.246       1
      4      AGE continuo…    median     Median     77       1
      5      AGE continuo…       p25         Q1     70       1
      6      AGE continuo…       p75         Q3     81       1
      7      AGE continuo…       min        Min     51       1
      8      AGE continuo…       max        Max     89       1
      # ... with 2 more variables: warning, error

---

    Code
      print.card(categorical_card)
    Output
      {cards} data frame: 9 x 9
        variable variable_level   context stat_name stat_label  stat
      1   AGEGR1          65-80 categori…         n          n   144
      2   AGEGR1          65-80 categori…         N          N   254
      3   AGEGR1          65-80 categori…         p          % 0.567
      4   AGEGR1            <65 categori…         n          n    33
      5   AGEGR1            <65 categori…         N          N   254
      6   AGEGR1            <65 categori…         p          %  0.13
      7   AGEGR1            >80 categori…         n          n    77
      8   AGEGR1            >80 categori…         N          N   254
      9   AGEGR1            >80 categori…         p          % 0.303
      # ... with 3 more variables: fmt_fun, warning, error

---

    Code
      print.card(grouped_card)
    Output
      {cards} data frame: 24 x 10
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
      # ... with 14 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

---

    Code
      print.card(formatted_card)
    Output
      {cards} data frame: 8 x 8
        variable   context stat_name stat_label   stat fmt_fun
      1      AGE continuo…         N          N    254    <fn>
      2      AGE continuo…      mean       Mean 75.087    <fn>
      3      AGE continuo…        sd         SD  8.246    <fn>
      4      AGE continuo…    median     Median     77    <fn>
      5      AGE continuo…       p25         Q1     70    <fn>
      6      AGE continuo…       p75         Q3     81    <fn>
      7      AGE continuo…       min        Min     51    <fn>
      8      AGE continuo…       max        Max     89    <fn>
      # ... with 2 more variables: warning, error

# print.card() width awareness works

    Code
      print.card(card_obj, width = 40)
    Output
      {cards} data frame: 24 x 10
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
      # ... with 14 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

---

    Code
      print.card(card_obj, width = 120)
    Output
      {cards} data frame: 24 x 10
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
      # ... with 14 more rows
      # ... with 4 more variables: context, fmt_fun, warning, error

