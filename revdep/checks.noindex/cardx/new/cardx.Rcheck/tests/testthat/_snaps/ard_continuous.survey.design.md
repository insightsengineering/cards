# unstratified ard_continuous.survey.design() works

    Code
      ard_uni_svy_cont
    Message
      {cards} data frame: 10 x 8
    Output
         variable   context      stat_name stat_label     stat fmt_fn
      1     api00 continuo…           mean       Mean  644.169      1
      2     api00 continuo…         median     Median      652      1
      3     api00 continuo…            min    Minimum      411      1
      4     api00 continuo…            max    Maximum      905      1
      5     api00 continuo…            sum        Sum  3989985      1
      6     api00 continuo…            var   Variance 11182.82      1
      7     api00 continuo…             sd  Standard…  105.749      1
      8     api00 continuo… mean.std.error   SE(Mean)   23.542      1
      9     api00 continuo…           deff  Design E…    9.346      1
      10    api00 continuo…            p75  75% Perc…      719      1
    Message
      i 2 more variables: warning, error

# ard_continuous.survey.design(fmt_fn)

    Code
      as.data.frame(dplyr::select(ard_continuous(dclus1, variables = c(api99, api00),
      statistic = ~ c("mean", "median", "min", "max"), fmt_fn = list(api00 = list(
        mean = 2, median = "xx.xx", min = as.character))), -warning, -error))
    Output
        variable    context stat_name stat_label     stat                     fmt_fn
      1    api99 continuous      mean       Mean 606.9781                          1
      2    api99 continuous    median     Median      615                          1
      3    api99 continuous       min    Minimum      365                          1
      4    api99 continuous       max    Maximum      890                          1
      5    api00 continuous      mean       Mean 644.1694                          2
      6    api00 continuous    median     Median      652                      xx.xx
      7    api00 continuous       min    Minimum      411 .Primitive("as.character")
      8    api00 continuous       max    Maximum      905                          1

# ard_continuous.survey.design(stat_label)

    Code
      as.data.frame(ard_continuous(dclus1, variables = c(api00, api99), statistic = ~
      c("mean", "median", "min", "max"), stat_label = list(api00 = list(mean = "MeAn",
        median = "MEDian", min = "MINimum"))))
    Output
        variable    context stat_name stat_label     stat fmt_fn warning error
      1    api00 continuous      mean       MeAn 644.1694      1    NULL  NULL
      2    api00 continuous    median     MEDian      652      1    NULL  NULL
      3    api00 continuous       min    MINimum      411      1    NULL  NULL
      4    api00 continuous       max    Maximum      905      1    NULL  NULL
      5    api99 continuous      mean       Mean 606.9781      1    NULL  NULL
      6    api99 continuous    median     Median      615      1    NULL  NULL
      7    api99 continuous       min    Minimum      365      1    NULL  NULL
      8    api99 continuous       max    Maximum      890      1    NULL  NULL

