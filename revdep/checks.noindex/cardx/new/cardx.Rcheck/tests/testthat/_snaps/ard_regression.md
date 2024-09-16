# ard_regression() works

    Code
      dplyr::mutate(dplyr::select(as.data.frame(ard_regression(lm(AGE ~ ARM, data = cards::ADSL),
      add_estimate_to_reference_rows = TRUE)), -context, -stat_label, -fmt_fn), stat = lapply(
        stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x)))
    Output
         variable       variable_level      stat_name                       stat
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
      12      ARM Xanomeline High Dose           term    ARMXanomeline High Dose
      13      ARM Xanomeline High Dose      var_label Description of Planned Arm
      14      ARM Xanomeline High Dose      var_class                  character
      15      ARM Xanomeline High Dose       var_type                categorical
      16      ARM Xanomeline High Dose    var_nlevels                          3
      17      ARM Xanomeline High Dose      contrasts            contr.treatment
      18      ARM Xanomeline High Dose contrasts_type                  treatment
      19      ARM Xanomeline High Dose  reference_row                      FALSE
      20      ARM Xanomeline High Dose          label       Xanomeline High Dose
      21      ARM Xanomeline High Dose          n_obs                         84
      22      ARM Xanomeline High Dose       estimate                     -0.828
      23      ARM Xanomeline High Dose      std.error                      1.267
      24      ARM Xanomeline High Dose      statistic                     -0.654
      25      ARM Xanomeline High Dose        p.value                      0.514
      26      ARM Xanomeline High Dose       conf.low                     -3.324
      27      ARM Xanomeline High Dose      conf.high                      1.668
      28      ARM  Xanomeline Low Dose           term     ARMXanomeline Low Dose
      29      ARM  Xanomeline Low Dose      var_label Description of Planned Arm
      30      ARM  Xanomeline Low Dose      var_class                  character
      31      ARM  Xanomeline Low Dose       var_type                categorical
      32      ARM  Xanomeline Low Dose    var_nlevels                          3
      33      ARM  Xanomeline Low Dose      contrasts            contr.treatment
      34      ARM  Xanomeline Low Dose contrasts_type                  treatment
      35      ARM  Xanomeline Low Dose  reference_row                      FALSE
      36      ARM  Xanomeline Low Dose          label        Xanomeline Low Dose
      37      ARM  Xanomeline Low Dose          n_obs                         84
      38      ARM  Xanomeline Low Dose       estimate                      0.457
      39      ARM  Xanomeline Low Dose      std.error                      1.267
      40      ARM  Xanomeline Low Dose      statistic                      0.361
      41      ARM  Xanomeline Low Dose        p.value                      0.719
      42      ARM  Xanomeline Low Dose       conf.low                     -2.039
      43      ARM  Xanomeline Low Dose      conf.high                      2.953

# ard_regression() works specifying custom tidier

    Code
      dplyr::mutate(dplyr::filter(dplyr::select(as.data.frame(ard_regression(lme4::lmer(
        mpg ~ hp + (1 | cyl), data = mtcars), tidy_fun = broom.mixed::tidy)),
      -context, -stat_label, -fmt_fn), map_lgl(stat, is.numeric)), stat = lapply(stat,
        function(x) ifelse(is.numeric(x), cards::round5(x, 3), x)))
    Output
                        variable           variable_level stat_name   stat
      1                       hp                     <NA>     n_obs     32
      2                       hp                     <NA>  estimate  -0.03
      3                       hp                     <NA> std.error  0.015
      4                       hp                     <NA> statistic -2.088
      5                       hp                     <NA>  conf.low -0.059
      6                       hp                     <NA> conf.high -0.002
      7      cyl.sd__(Intercept)      cyl.sd__(Intercept)  estimate  4.023
      8 Residual.sd__Observation Residual.sd__Observation  estimate  3.149

