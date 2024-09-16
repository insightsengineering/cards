# ard_regression_basic() works

    Code
      dplyr::select(as.data.frame(ard), -fmt_fn)
    Output
         variable       variable_level    context stat_name     stat_label       stat
      1       ARM Xanomeline High Dose regression  estimate    Coefficient -0.8283499
      2       ARM Xanomeline High Dose regression std.error Standard Error   1.267394
      3       ARM Xanomeline High Dose regression statistic      statistic  -0.653585
      4       ARM Xanomeline High Dose regression   p.value        p-value  0.5139775
      5       ARM Xanomeline High Dose regression  conf.low CI Lower Bound  -3.324433
      6       ARM Xanomeline High Dose regression conf.high CI Upper Bound   1.667733
      7       ARM  Xanomeline Low Dose regression  estimate    Coefficient  0.4573643
      8       ARM  Xanomeline Low Dose regression std.error Standard Error   1.267394
      9       ARM  Xanomeline Low Dose regression statistic      statistic  0.3608698
      10      ARM  Xanomeline Low Dose regression   p.value        p-value  0.7185003
      11      ARM  Xanomeline Low Dose regression  conf.low CI Lower Bound  -2.038718
      12      ARM  Xanomeline Low Dose regression conf.high CI Upper Bound   2.953447

