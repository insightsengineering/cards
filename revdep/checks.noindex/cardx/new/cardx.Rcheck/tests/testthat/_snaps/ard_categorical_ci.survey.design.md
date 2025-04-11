# ard_categorical_ci(data)

    Code
      dplyr::select(ard_categorical_ci(dclus1, variables = c(both, awards)), -warning,
      -error, -fmt_fn, -context)
    Message
      {cards} data frame: 20 x 5
    Output
         variable variable_level  stat_name stat_label  stat
      1      both             No   estimate   estimate 0.273
      2      both             No   conf.low   conf.low 0.213
      3      both             No  conf.high  conf.high 0.343
      4      both             No     method     method logit
      5      both             No conf.level  conf.lev…  0.95
      6      both            Yes   estimate   estimate 0.727
      7      both            Yes   conf.low   conf.low 0.657
      8      both            Yes  conf.high  conf.high 0.787
      9      both            Yes     method     method logit
      10     both            Yes conf.level  conf.lev…  0.95
      11   awards             No   estimate   estimate  0.29
      12   awards             No   conf.low   conf.low 0.224
      13   awards             No  conf.high  conf.high 0.365
      14   awards             No     method     method logit
      15   awards             No conf.level  conf.lev…  0.95
      16   awards            Yes   estimate   estimate  0.71
      17   awards            Yes   conf.low   conf.low 0.635
      18   awards            Yes  conf.high  conf.high 0.776
      19   awards            Yes     method     method logit
      20   awards            Yes conf.level  conf.lev…  0.95

