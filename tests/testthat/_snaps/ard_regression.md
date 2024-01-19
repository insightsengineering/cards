# ard_regression() works

    Code
      as.data.frame(dplyr::mutate(ard_regression(lm(AGE ~ ARM, data = ADSL),
      add_estimate_to_reference_rows = TRUE), statistic = lapply(statistic, function(
        x) ifelse(is.numeric(x), round5(x, 3), x))))
    Output
         variable       variable_level    context      stat_name     stat_label
      1       ARM              Placebo regression           term           term
      2       ARM              Placebo regression      var_label          Label
      3       ARM              Placebo regression      var_class          Class
      4       ARM              Placebo regression       var_type           Type
      5       ARM              Placebo regression    var_nlevels       N Levels
      6       ARM              Placebo regression      contrasts      contrasts
      7       ARM              Placebo regression contrasts_type  Contrast Type
      8       ARM              Placebo regression  reference_row  reference_row
      9       ARM              Placebo regression          label    Level Label
      10      ARM              Placebo regression          n_obs         N Obs.
      11      ARM              Placebo regression       estimate    Coefficient
      12      ARM Xanomeline High Dose regression           term           term
      13      ARM Xanomeline High Dose regression      var_label          Label
      14      ARM Xanomeline High Dose regression      var_class          Class
      15      ARM Xanomeline High Dose regression       var_type           Type
      16      ARM Xanomeline High Dose regression    var_nlevels       N Levels
      17      ARM Xanomeline High Dose regression      contrasts      contrasts
      18      ARM Xanomeline High Dose regression contrasts_type  Contrast Type
      19      ARM Xanomeline High Dose regression  reference_row  reference_row
      20      ARM Xanomeline High Dose regression          label    Level Label
      21      ARM Xanomeline High Dose regression          n_obs         N Obs.
      22      ARM Xanomeline High Dose regression       estimate    Coefficient
      23      ARM Xanomeline High Dose regression      std.error Standard Error
      24      ARM Xanomeline High Dose regression      statistic      statistic
      25      ARM Xanomeline High Dose regression        p.value        p-value
      26      ARM Xanomeline High Dose regression       conf.low CI Lower Bound
      27      ARM Xanomeline High Dose regression      conf.high CI Upper Bound
      28      ARM  Xanomeline Low Dose regression           term           term
      29      ARM  Xanomeline Low Dose regression      var_label          Label
      30      ARM  Xanomeline Low Dose regression      var_class          Class
      31      ARM  Xanomeline Low Dose regression       var_type           Type
      32      ARM  Xanomeline Low Dose regression    var_nlevels       N Levels
      33      ARM  Xanomeline Low Dose regression      contrasts      contrasts
      34      ARM  Xanomeline Low Dose regression contrasts_type  Contrast Type
      35      ARM  Xanomeline Low Dose regression  reference_row  reference_row
      36      ARM  Xanomeline Low Dose regression          label    Level Label
      37      ARM  Xanomeline Low Dose regression          n_obs         N Obs.
      38      ARM  Xanomeline Low Dose regression       estimate    Coefficient
      39      ARM  Xanomeline Low Dose regression      std.error Standard Error
      40      ARM  Xanomeline Low Dose regression      statistic      statistic
      41      ARM  Xanomeline Low Dose regression        p.value        p-value
      42      ARM  Xanomeline Low Dose regression       conf.low CI Lower Bound
      43      ARM  Xanomeline Low Dose regression      conf.high CI Upper Bound
                          statistic statistic_fmt_fn
      1                  ARMPlacebo             NULL
      2  Description of Planned Arm             NULL
      3                   character             NULL
      4                 categorical             NULL
      5                           3                0
      6             contr.treatment             NULL
      7                   treatment             NULL
      8                        TRUE             NULL
      9                     Placebo             NULL
      10                         86                1
      11                          0                1
      12    ARMXanomeline High Dose             NULL
      13 Description of Planned Arm             NULL
      14                  character             NULL
      15                categorical             NULL
      16                          3                0
      17            contr.treatment             NULL
      18                  treatment             NULL
      19                      FALSE             NULL
      20       Xanomeline High Dose             NULL
      21                         84                1
      22                     -0.828                1
      23                      1.267                1
      24                     -0.654                1
      25                      0.514                1
      26                     -3.324                1
      27                      1.668                1
      28     ARMXanomeline Low Dose             NULL
      29 Description of Planned Arm             NULL
      30                  character             NULL
      31                categorical             NULL
      32                          3                0
      33            contr.treatment             NULL
      34                  treatment             NULL
      35                      FALSE             NULL
      36        Xanomeline Low Dose             NULL
      37                         84                1
      38                      0.457                1
      39                      1.267                1
      40                      0.361                1
      41                      0.719                1
      42                     -2.039                1
      43                      2.953                1

