# ard_stats_mood_test() works

    Code
      as.data.frame(ard_stats_mood_test(cards::ADSL, by = SEX, variable = AGE))
    Output
        group1 variable         context   stat_name             stat_label
      1    SEX      AGE stats_mood_test   statistic            Z-Statistic
      2    SEX      AGE stats_mood_test     p.value                p-value
      3    SEX      AGE stats_mood_test      method                 method
      4    SEX      AGE stats_mood_test alternative Alternative Hypothesis
                                 stat fmt_fn warning error
      1                     0.1292194      1    NULL  NULL
      2                     0.8971841      1    NULL  NULL
      3 Mood two-sample test of scale   NULL    NULL  NULL
      4                     two.sided   NULL    NULL  NULL

