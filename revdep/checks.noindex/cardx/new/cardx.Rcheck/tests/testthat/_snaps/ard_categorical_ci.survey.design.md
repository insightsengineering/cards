# ard_categorical_ci(data)

    Code
      dplyr::select(as.data.frame(ard_categorical_ci(dclus1, variables = c(both,
        awards))), -warning, -error, -fmt_fn, -context)
    Output
         variable variable_level  stat_name stat_label      stat
      1      both             No   estimate   estimate  0.273224
      2      both             No   conf.low   conf.low 0.2131745
      3      both             No  conf.high  conf.high  0.342819
      4      both             No     method     method     logit
      5      both             No conf.level conf.level      0.95
      6      both            Yes   estimate   estimate  0.726776
      7      both            Yes   conf.low   conf.low  0.657181
      8      both            Yes  conf.high  conf.high 0.7868255
      9      both            Yes     method     method     logit
      10     both            Yes conf.level conf.level      0.95
      11   awards             No   estimate   estimate 0.2896175
      12   awards             No   conf.low   conf.low 0.2241835
      13   awards             No  conf.high  conf.high 0.3651608
      14   awards             No     method     method     logit
      15   awards             No conf.level conf.level      0.95
      16   awards            Yes   estimate   estimate 0.7103825
      17   awards            Yes   conf.low   conf.low 0.6348392
      18   awards            Yes  conf.high  conf.high 0.7758165
      19   awards            Yes     method     method     logit
      20   awards            Yes conf.level conf.level      0.95

