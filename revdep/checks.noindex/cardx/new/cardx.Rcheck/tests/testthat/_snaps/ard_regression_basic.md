# ard_regression_basic() works

    Code
      dplyr::select(as.data.frame(ard), -fmt_fn)
    Output
         variable       variable_level    context stat_name     stat_label       stat warning error
      1       ARM Xanomeline High Dose regression  estimate    Coefficient -0.8283499    NULL  NULL
      2       ARM Xanomeline High Dose regression std.error Standard Error   1.267394    NULL  NULL
      3       ARM Xanomeline High Dose regression statistic      statistic  -0.653585    NULL  NULL
      4       ARM Xanomeline High Dose regression   p.value        p-value  0.5139775    NULL  NULL
      5       ARM Xanomeline High Dose regression  conf.low CI Lower Bound  -3.324433    NULL  NULL
      6       ARM Xanomeline High Dose regression conf.high CI Upper Bound   1.667733    NULL  NULL
      7       ARM  Xanomeline Low Dose regression  estimate    Coefficient  0.4573643    NULL  NULL
      8       ARM  Xanomeline Low Dose regression std.error Standard Error   1.267394    NULL  NULL
      9       ARM  Xanomeline Low Dose regression statistic      statistic  0.3608698    NULL  NULL
      10      ARM  Xanomeline Low Dose regression   p.value        p-value  0.7185003    NULL  NULL
      11      ARM  Xanomeline Low Dose regression  conf.low CI Lower Bound  -2.038718    NULL  NULL
      12      ARM  Xanomeline Low Dose regression conf.high CI Upper Bound   2.953447    NULL  NULL

