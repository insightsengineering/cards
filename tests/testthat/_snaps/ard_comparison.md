# ard_ttest() works

    Code
      as.data.frame(ard_ttest(ADSL, by = ARM, variable = AGE, var.equal = TRUE))
    Output
         group1 group1_level variable context   stat_name          stat_label
      1     ARM         NULL      AGE   ttest    estimate     Mean Difference
      2     ARM         NULL      AGE   ttest   estimate1        Group 1 Mean
      3     ARM         NULL      AGE   ttest   estimate2        Group 2 Mean
      4     ARM         NULL      AGE   ttest   statistic         t Statistic
      5     ARM         NULL      AGE   ttest     p.value             p-value
      6     ARM         NULL      AGE   ttest   parameter  Degrees of Freedom
      7     ARM         NULL      AGE   ttest    conf.low      CI Lower Bound
      8     ARM         NULL      AGE   ttest   conf.high      CI Upper Bound
      9     ARM         NULL      AGE   ttest      method              method
      10    ARM         NULL      AGE   ttest alternative         alternative
      11    ARM         NULL      AGE   ttest          mu             H0 Mean
      12    ARM         NULL      AGE   ttest      paired       Paired t-test
      13    ARM         NULL      AGE   ttest   var.equal     Equal Variances
      14    ARM         NULL      AGE   ttest  conf.level CI Confidence Level
         statistic statistic_fmt_fn warning
      1       NULL             NULL    NULL
      2       NULL             NULL    NULL
      3       NULL             NULL    NULL
      4       NULL             NULL    NULL
      5       NULL             NULL    NULL
      6       NULL             NULL    NULL
      7       NULL             NULL    NULL
      8       NULL             NULL    NULL
      9       NULL             NULL    NULL
      10      NULL             NULL    NULL
      11         0                1    NULL
      12     FALSE             NULL    NULL
      13      TRUE             NULL    NULL
      14      0.95                1    NULL
                                              error
      1  grouping factor must have exactly 2 levels
      2  grouping factor must have exactly 2 levels
      3  grouping factor must have exactly 2 levels
      4  grouping factor must have exactly 2 levels
      5  grouping factor must have exactly 2 levels
      6  grouping factor must have exactly 2 levels
      7  grouping factor must have exactly 2 levels
      8  grouping factor must have exactly 2 levels
      9  grouping factor must have exactly 2 levels
      10 grouping factor must have exactly 2 levels
      11 grouping factor must have exactly 2 levels
      12 grouping factor must have exactly 2 levels
      13 grouping factor must have exactly 2 levels
      14 grouping factor must have exactly 2 levels

