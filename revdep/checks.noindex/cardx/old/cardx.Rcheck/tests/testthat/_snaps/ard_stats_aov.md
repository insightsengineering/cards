# ard_aov() works

    Code
      as.data.frame(ard_stats_aov(AGE ~ ARM + SEX, data = cards::ADSL))
    Output
         variable   context stat_name             stat_label      stat warning error
      1       ARM stats_aov     sumsq         Sum of Squares  71.38574    NULL  NULL
      2       ARM stats_aov        df     Degrees of Freedom         2    NULL  NULL
      3       ARM stats_aov    meansq Mean of Sum of Squares  35.69287    NULL  NULL
      4       ARM stats_aov statistic              Statistic 0.5235002    NULL  NULL
      5       ARM stats_aov   p.value                p-value 0.5930912    NULL  NULL
      6       SEX stats_aov     sumsq         Sum of Squares  87.40947    NULL  NULL
      7       SEX stats_aov        df     Degrees of Freedom         1    NULL  NULL
      8       SEX stats_aov    meansq Mean of Sum of Squares  87.40947    NULL  NULL
      9       SEX stats_aov statistic              Statistic  1.282017    NULL  NULL
      10      SEX stats_aov   p.value                p-value 0.2586091    NULL  NULL

