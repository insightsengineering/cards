# ard_aov() works

    Code
      as.data.frame(ard_stats_aov(AGE ~ ARM + SEX, data = cards::ADSL))
    Output
         variable   context stat_name             stat_label      stat fmt_fn warning
      1       ARM stats_aov     sumsq         Sum of Squares  71.38574      1    NULL
      2       ARM stats_aov        df     Degrees of Freedom         2      1    NULL
      3       ARM stats_aov    meansq Mean of Sum of Squares  35.69287      1    NULL
      4       ARM stats_aov statistic              Statistic 0.5235002      1    NULL
      5       ARM stats_aov   p.value                p-value 0.5930912      1    NULL
      6       SEX stats_aov     sumsq         Sum of Squares  87.40947      1    NULL
      7       SEX stats_aov        df     Degrees of Freedom         1      1    NULL
      8       SEX stats_aov    meansq Mean of Sum of Squares  87.40947      1    NULL
      9       SEX stats_aov statistic              Statistic  1.282017      1    NULL
      10      SEX stats_aov   p.value                p-value 0.2586091      1    NULL
         error
      1   NULL
      2   NULL
      3   NULL
      4   NULL
      5   NULL
      6   NULL
      7   NULL
      8   NULL
      9   NULL
      10  NULL

