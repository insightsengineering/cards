# ard_stats_oneway_test() works

    Code
      head(dplyr::select(ard_stats_oneway_test(AGEGR1 ~ ARM, data = cards::ADSL), c(
        "stat_name", "stat", "warning")), 3)
    Message
      {cards} data frame: 3 x 3
    Output
        stat_name stat   warning
      1    num.df    2 argument…
      2    den.df   NA argument…
      3 statistic   NA argument…

