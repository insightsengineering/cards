# ard_total_n.survey.design() works

    Code
      ard_total_n(survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~
      Freq))
    Message
      {cards} data frame: 2 x 8
    Output
               variable context    stat_name stat_label stat fmt_fn
      1 ..ard_total_n.. total_n            N          N 2201   <fn>
      2 ..ard_total_n.. total_n N_unweighted  Unweight…   32   <fn>
    Message
      i 2 more variables: warning, error

