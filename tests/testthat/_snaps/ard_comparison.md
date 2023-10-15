# ard_ttest() works

    Code
      as.data.frame(ard_ttest(ADSL, by = ARM, variable = AGE, var.equal = TRUE))
    Output
         group1 group1_level variable context   stat_name statistic warning
      1     ARM         NULL      AGE   ttest    estimate      NULL    NULL
      2     ARM         NULL      AGE   ttest   estimate1      NULL    NULL
      3     ARM         NULL      AGE   ttest   estimate2      NULL    NULL
      4     ARM         NULL      AGE   ttest   statistic      NULL    NULL
      5     ARM         NULL      AGE   ttest     p.value      NULL    NULL
      6     ARM         NULL      AGE   ttest   parameter      NULL    NULL
      7     ARM         NULL      AGE   ttest    conf.low      NULL    NULL
      8     ARM         NULL      AGE   ttest   conf.high      NULL    NULL
      9     ARM         NULL      AGE   ttest      method      NULL    NULL
      10    ARM         NULL      AGE   ttest alternative      NULL    NULL
      11    ARM         NULL      AGE   ttest          mu         0    NULL
      12    ARM         NULL      AGE   ttest      paired     FALSE    NULL
      13    ARM         NULL      AGE   ttest   var.equal      TRUE    NULL
      14    ARM         NULL      AGE   ttest  conf.level      0.95    NULL
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

