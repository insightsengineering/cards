# ard_stats_kruskal_test() works

    Code
      as.data.frame(ard_stats_kruskal_test(cards::ADSL, by = "ARM", variables = "AGE"))
    Output
        group1 variable            context stat_name
      1    ARM      AGE stats_kruskal_test statistic
      2    ARM      AGE stats_kruskal_test   p.value
      3    ARM      AGE stats_kruskal_test parameter
      4    ARM      AGE stats_kruskal_test    method
                                  stat_label                         stat fmt_fn
      1 Kruskal-Wallis chi-squared Statistic                      1.63473      1
      2                              p-value                    0.4415937      1
      3                   Degrees of Freedom                            2      1
      4                               method Kruskal-Wallis rank sum test   NULL
        warning error
      1    NULL  NULL
      2    NULL  NULL
      3    NULL  NULL
      4    NULL  NULL

