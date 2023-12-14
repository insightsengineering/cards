# ard_regression() works

    Code
      as.data.frame(flatten_ard(dplyr::mutate(ard_regression(lm(AGE ~ ARM, data = ADSL),
      add_estimate_to_reference_rows = TRUE), statistic = lapply(statistic, function(
        x) ifelse(is.numeric(x), round5(x, 3), x)))))
    Output
         variable       variable_level      stat_name                  statistic
      1       ARM              Placebo           term                 ARMPlacebo
      2       ARM              Placebo      var_label Description of Planned Arm
      3       ARM              Placebo      var_class                  character
      4       ARM              Placebo       var_type                categorical
      5       ARM              Placebo    var_nlevels                          3
      6       ARM              Placebo      contrasts            contr.treatment
      7       ARM              Placebo contrasts_type                  treatment
      8       ARM              Placebo  reference_row                       TRUE
      9       ARM              Placebo          label                    Placebo
      10      ARM              Placebo          n_obs                         86
      11      ARM              Placebo       estimate                          0
      12      ARM              Placebo      std.error                       <NA>
      13      ARM              Placebo      statistic                       <NA>
      14      ARM              Placebo        p.value                       <NA>
      15      ARM              Placebo       conf.low                       <NA>
      16      ARM              Placebo      conf.high                       <NA>
      17      ARM Xanomeline High Dose           term    ARMXanomeline High Dose
      18      ARM Xanomeline High Dose      var_label Description of Planned Arm
      19      ARM Xanomeline High Dose      var_class                  character
      20      ARM Xanomeline High Dose       var_type                categorical
      21      ARM Xanomeline High Dose    var_nlevels                          3
      22      ARM Xanomeline High Dose      contrasts            contr.treatment
      23      ARM Xanomeline High Dose contrasts_type                  treatment
      24      ARM Xanomeline High Dose  reference_row                      FALSE
      25      ARM Xanomeline High Dose          label       Xanomeline High Dose
      26      ARM Xanomeline High Dose          n_obs                         84
      27      ARM Xanomeline High Dose       estimate                     -0.828
      28      ARM Xanomeline High Dose      std.error                      1.267
      29      ARM Xanomeline High Dose      statistic                     -0.654
      30      ARM Xanomeline High Dose        p.value                      0.514
      31      ARM Xanomeline High Dose       conf.low                     -3.324
      32      ARM Xanomeline High Dose      conf.high                      1.668
      33      ARM  Xanomeline Low Dose           term     ARMXanomeline Low Dose
      34      ARM  Xanomeline Low Dose      var_label Description of Planned Arm
      35      ARM  Xanomeline Low Dose      var_class                  character
      36      ARM  Xanomeline Low Dose       var_type                categorical
      37      ARM  Xanomeline Low Dose    var_nlevels                          3
      38      ARM  Xanomeline Low Dose      contrasts            contr.treatment
      39      ARM  Xanomeline Low Dose contrasts_type                  treatment
      40      ARM  Xanomeline Low Dose  reference_row                      FALSE
      41      ARM  Xanomeline Low Dose          label        Xanomeline Low Dose
      42      ARM  Xanomeline Low Dose          n_obs                         84
      43      ARM  Xanomeline Low Dose       estimate                      0.457
      44      ARM  Xanomeline Low Dose      std.error                      1.267
      45      ARM  Xanomeline Low Dose      statistic                      0.361
      46      ARM  Xanomeline Low Dose        p.value                      0.719
      47      ARM  Xanomeline Low Dose       conf.low                     -2.039
      48      ARM  Xanomeline Low Dose      conf.high                      2.953

