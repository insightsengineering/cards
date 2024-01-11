# ard_proportion_ci(method='strat_wilson') works

    Code
      ard_proportion_ci(data = data.frame(rsp = rsp, strata = interaction(strata_data)),
      variables = rsp, strata = strata, weights = weights, method = "strat_wilson")
    Message
      {cards} data frame: 6 x 8
    Output
        variable   context  stat_name stat_label statistic statistic_fmt_fn
      1      rsp proporti…          N          N        80                0
      2      rsp proporti…   estimate   estimate     0.625                1
      3      rsp proporti…   conf.low   conf.low     0.487                1
      4      rsp proporti…  conf.high  conf.high     0.719                1
      5      rsp proporti… conf.level  conf.lev…      0.95                1
      6      rsp proporti…     method     method Stratifi…                1
    Message
      i 2 more variables: warning, error

---

    Code
      ard_proportion_ci(data = data.frame(rsp = rsp, strata = interaction(strata_data)),
      variables = rsp, strata = strata, weights = weights, method = "strat_wilsoncc")
    Message
      {cards} data frame: 6 x 8
    Output
        variable   context  stat_name stat_label statistic statistic_fmt_fn
      1      rsp proporti…          N          N        80                0
      2      rsp proporti…   estimate   estimate     0.625                1
      3      rsp proporti…   conf.low   conf.low     0.448                1
      4      rsp proporti…  conf.high  conf.high     0.753                1
      5      rsp proporti… conf.level  conf.lev…      0.95                1
      6      rsp proporti…     method     method Stratifi…                1
    Message
      i 2 more variables: warning, error

