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
      12      ARM Xanomeline High Dose           term           term
      13      ARM Xanomeline High Dose      var_label          Label
      14      ARM Xanomeline High Dose      var_class          Class
      15      ARM Xanomeline High Dose       var_type           Type
      16      ARM Xanomeline High Dose    var_nlevels       N Levels
      17      ARM Xanomeline High Dose      contrasts      contrasts
      18      ARM Xanomeline High Dose contrasts_type  Contrast Type
      19      ARM Xanomeline High Dose  reference_row  reference_row
      20      ARM Xanomeline High Dose          label    Level Label
      21      ARM Xanomeline High Dose          n_obs         N Obs.
      22      ARM Xanomeline High Dose       estimate    Coefficient
      23      ARM Xanomeline High Dose      std.error Standard Error
      24      ARM Xanomeline High Dose      statistic      statistic
      25      ARM Xanomeline High Dose        p.value        p-value
      26      ARM Xanomeline High Dose       conf.low CI Lower Bound
      27      ARM Xanomeline High Dose      conf.high CI Upper Bound
      28      ARM  Xanomeline Low Dose           term           term
      29      ARM  Xanomeline Low Dose      var_label          Label
      30      ARM  Xanomeline Low Dose      var_class          Class
      31      ARM  Xanomeline Low Dose       var_type           Type
      32      ARM  Xanomeline Low Dose    var_nlevels       N Levels
      33      ARM  Xanomeline Low Dose      contrasts      contrasts
      34      ARM  Xanomeline Low Dose contrasts_type  Contrast Type
      35      ARM  Xanomeline Low Dose  reference_row  reference_row
      36      ARM  Xanomeline Low Dose          label    Level Label
      37      ARM  Xanomeline Low Dose          n_obs         N Obs.
      38      ARM  Xanomeline Low Dose       estimate    Coefficient
      39      ARM  Xanomeline Low Dose      std.error Standard Error
      40      ARM  Xanomeline Low Dose      statistic      statistic
      41      ARM  Xanomeline Low Dose        p.value        p-value
      42      ARM  Xanomeline Low Dose       conf.low CI Lower Bound
      43      ARM  Xanomeline Low Dose      conf.high CI Upper Bound
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
      12    ARMXanomeline High Dose             <NA>
      13 Description of Planned Arm             <NA>
      14                  character             <NA>
      15                categorical             <NA>
      16                          3                0
      17            contr.treatment             <NA>
      18                  treatment             <NA>
      19                      FALSE             <NA>
      20       Xanomeline High Dose             <NA>
      21                         84                1
      22                     -0.828                1
      23                      1.267                1
      24                     -0.654                1
      25                      0.514                1
      26                     -3.324                1
      27                      1.668                1
      28     ARMXanomeline Low Dose             <NA>
      29 Description of Planned Arm             <NA>
      30                  character             <NA>
      31                categorical             <NA>
      32                          3                0
      33            contr.treatment             <NA>
      34                  treatment             <NA>
      35                      FALSE             <NA>
      36        Xanomeline Low Dose             <NA>
      37                         84                1
      38                      0.457                1
      39                      1.267                1
      40                      0.361                1
      41                      0.719                1
      42                     -2.039                1
      43                      2.953                1

