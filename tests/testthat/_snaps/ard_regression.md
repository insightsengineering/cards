# ard_regression() works

    Code
      as.data.frame(flatten_ard(dplyr::mutate(ard_regression(lm(AGE ~ ARM, data = ADSL),
      add_estimate_to_reference_rows = TRUE), statistic = lapply(statistic, function(
        x) ifelse(is.numeric(x), round5(x, 3), x)))))
    Output
         variable       variable_level      stat_name     stat_label
      1       ARM              Placebo           term           term
      2       ARM              Placebo      var_label          Label
      3       ARM              Placebo      var_class          Class
      4       ARM              Placebo       var_type           Type
      5       ARM              Placebo    var_nlevels       N Levels
      6       ARM              Placebo      contrasts      contrasts
      7       ARM              Placebo contrasts_type  Contrast Type
      8       ARM              Placebo  reference_row  reference_row
      9       ARM              Placebo          label    Level Label
      10      ARM              Placebo          n_obs         N Obs.
      11      ARM              Placebo       estimate    Coefficient
      12      ARM              Placebo      std.error Standard Error
      13      ARM              Placebo      statistic      statistic
      14      ARM              Placebo        p.value        p-value
      15      ARM              Placebo       conf.low CI Lower Bound
      16      ARM              Placebo      conf.high CI Upper Bound
      17      ARM Xanomeline High Dose           term           term
      18      ARM Xanomeline High Dose      var_label          Label
      19      ARM Xanomeline High Dose      var_class          Class
      20      ARM Xanomeline High Dose       var_type           Type
      21      ARM Xanomeline High Dose    var_nlevels       N Levels
      22      ARM Xanomeline High Dose      contrasts      contrasts
      23      ARM Xanomeline High Dose contrasts_type  Contrast Type
      24      ARM Xanomeline High Dose  reference_row  reference_row
      25      ARM Xanomeline High Dose          label    Level Label
      26      ARM Xanomeline High Dose          n_obs         N Obs.
      27      ARM Xanomeline High Dose       estimate    Coefficient
      28      ARM Xanomeline High Dose      std.error Standard Error
      29      ARM Xanomeline High Dose      statistic      statistic
      30      ARM Xanomeline High Dose        p.value        p-value
      31      ARM Xanomeline High Dose       conf.low CI Lower Bound
      32      ARM Xanomeline High Dose      conf.high CI Upper Bound
      33      ARM  Xanomeline Low Dose           term           term
      34      ARM  Xanomeline Low Dose      var_label          Label
      35      ARM  Xanomeline Low Dose      var_class          Class
      36      ARM  Xanomeline Low Dose       var_type           Type
      37      ARM  Xanomeline Low Dose    var_nlevels       N Levels
      38      ARM  Xanomeline Low Dose      contrasts      contrasts
      39      ARM  Xanomeline Low Dose contrasts_type  Contrast Type
      40      ARM  Xanomeline Low Dose  reference_row  reference_row
      41      ARM  Xanomeline Low Dose          label    Level Label
      42      ARM  Xanomeline Low Dose          n_obs         N Obs.
      43      ARM  Xanomeline Low Dose       estimate    Coefficient
      44      ARM  Xanomeline Low Dose      std.error Standard Error
      45      ARM  Xanomeline Low Dose      statistic      statistic
      46      ARM  Xanomeline Low Dose        p.value        p-value
      47      ARM  Xanomeline Low Dose       conf.low CI Lower Bound
      48      ARM  Xanomeline Low Dose      conf.high CI Upper Bound
                          statistic statistic_fmt_fn
      1                  ARMPlacebo             <NA>
      2  Description of Planned Arm             <NA>
      3                   character             <NA>
      4                 categorical             <NA>
      5                           3                0
      6             contr.treatment             <NA>
      7                   treatment             <NA>
      8                        TRUE             <NA>
      9                     Placebo             <NA>
      10                         86                1
      11                          0                1
      12                       <NA>                1
      13                       <NA>                1
      14                       <NA>                1
      15                       <NA>                1
      16                       <NA>                1
      17    ARMXanomeline High Dose             <NA>
      18 Description of Planned Arm             <NA>
      19                  character             <NA>
      20                categorical             <NA>
      21                          3                0
      22            contr.treatment             <NA>
      23                  treatment             <NA>
      24                      FALSE             <NA>
      25       Xanomeline High Dose             <NA>
      26                         84                1
      27                     -0.828                1
      28                      1.267                1
      29                     -0.654                1
      30                      0.514                1
      31                     -3.324                1
      32                      1.668                1
      33     ARMXanomeline Low Dose             <NA>
      34 Description of Planned Arm             <NA>
      35                  character             <NA>
      36                categorical             <NA>
      37                          3                0
      38            contr.treatment             <NA>
      39                  treatment             <NA>
      40                      FALSE             <NA>
      41        Xanomeline Low Dose             <NA>
      42                         84                1
      43                      0.457                1
      44                      1.267                1
      45                      0.361                1
      46                      0.719                1
      47                     -2.039                1
      48                      2.953                1

