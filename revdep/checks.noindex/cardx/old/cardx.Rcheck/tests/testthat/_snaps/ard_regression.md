# ard_regression() works

    Code
      dplyr::mutate(dplyr::select(as.data.frame(ard_regression(lm(AGE ~ ARM, data = cards::ADSL),
      add_estimate_to_reference_rows = TRUE)), -context, -stat_label, -fmt_fn), stat = lapply(
        stat, function(x) ifelse(is.numeric(x), cards::round5(x, 3), x)))
    Output
         variable       variable_level      stat_name                       stat warning error
      1       ARM              Placebo           term                 ARMPlacebo    NULL  NULL
      2       ARM              Placebo      var_label Description of Planned Arm    NULL  NULL
      3       ARM              Placebo      var_class                  character    NULL  NULL
      4       ARM              Placebo       var_type                categorical    NULL  NULL
      5       ARM              Placebo    var_nlevels                          3    NULL  NULL
      6       ARM              Placebo      contrasts            contr.treatment    NULL  NULL
      7       ARM              Placebo contrasts_type                  treatment    NULL  NULL
      8       ARM              Placebo  reference_row                       TRUE    NULL  NULL
      9       ARM              Placebo          label                    Placebo    NULL  NULL
      10      ARM              Placebo          n_obs                         86    NULL  NULL
      11      ARM              Placebo       estimate                          0    NULL  NULL
      12      ARM Xanomeline High Dose           term    ARMXanomeline High Dose    NULL  NULL
      13      ARM Xanomeline High Dose      var_label Description of Planned Arm    NULL  NULL
      14      ARM Xanomeline High Dose      var_class                  character    NULL  NULL
      15      ARM Xanomeline High Dose       var_type                categorical    NULL  NULL
      16      ARM Xanomeline High Dose    var_nlevels                          3    NULL  NULL
      17      ARM Xanomeline High Dose      contrasts            contr.treatment    NULL  NULL
      18      ARM Xanomeline High Dose contrasts_type                  treatment    NULL  NULL
      19      ARM Xanomeline High Dose  reference_row                      FALSE    NULL  NULL
      20      ARM Xanomeline High Dose          label       Xanomeline High Dose    NULL  NULL
      21      ARM Xanomeline High Dose          n_obs                         84    NULL  NULL
      22      ARM Xanomeline High Dose       estimate                     -0.828    NULL  NULL
      23      ARM Xanomeline High Dose      std.error                      1.267    NULL  NULL
      24      ARM Xanomeline High Dose      statistic                     -0.654    NULL  NULL
      25      ARM Xanomeline High Dose        p.value                      0.514    NULL  NULL
      26      ARM Xanomeline High Dose       conf.low                     -3.324    NULL  NULL
      27      ARM Xanomeline High Dose      conf.high                      1.668    NULL  NULL
      28      ARM  Xanomeline Low Dose           term     ARMXanomeline Low Dose    NULL  NULL
      29      ARM  Xanomeline Low Dose      var_label Description of Planned Arm    NULL  NULL
      30      ARM  Xanomeline Low Dose      var_class                  character    NULL  NULL
      31      ARM  Xanomeline Low Dose       var_type                categorical    NULL  NULL
      32      ARM  Xanomeline Low Dose    var_nlevels                          3    NULL  NULL
      33      ARM  Xanomeline Low Dose      contrasts            contr.treatment    NULL  NULL
      34      ARM  Xanomeline Low Dose contrasts_type                  treatment    NULL  NULL
      35      ARM  Xanomeline Low Dose  reference_row                      FALSE    NULL  NULL
      36      ARM  Xanomeline Low Dose          label        Xanomeline Low Dose    NULL  NULL
      37      ARM  Xanomeline Low Dose          n_obs                         84    NULL  NULL
      38      ARM  Xanomeline Low Dose       estimate                      0.457    NULL  NULL
      39      ARM  Xanomeline Low Dose      std.error                      1.267    NULL  NULL
      40      ARM  Xanomeline Low Dose      statistic                      0.361    NULL  NULL
      41      ARM  Xanomeline Low Dose        p.value                      0.719    NULL  NULL
      42      ARM  Xanomeline Low Dose       conf.low                     -2.039    NULL  NULL
      43      ARM  Xanomeline Low Dose      conf.high                      2.953    NULL  NULL

# ard_regression() works specifying custom tidier

    Code
      dplyr::mutate(dplyr::filter(dplyr::select(as.data.frame(ard_regression(lme4::lmer(mpg ~
        hp + (1 | cyl), data = mtcars), tidy_fun = broom.mixed::tidy)), -context, -stat_label,
      -fmt_fn), map_lgl(stat, is.numeric)), stat = lapply(stat, function(x) ifelse(is.numeric(x),
      cards::round5(x, 3), x)))
    Output
                        variable           variable_level stat_name   stat warning error
      1                       hp                       NA     n_obs     32    NULL  NULL
      2                       hp                       NA  estimate  -0.03    NULL  NULL
      3                       hp                       NA std.error  0.015    NULL  NULL
      4                       hp                       NA statistic -2.088    NULL  NULL
      5                       hp                       NA  conf.low -0.059    NULL  NULL
      6                       hp                       NA conf.high -0.002    NULL  NULL
      7      cyl.sd__(Intercept)      cyl.sd__(Intercept)  estimate  4.023    NULL  NULL
      8 Residual.sd__Observation Residual.sd__Observation  estimate  3.149    NULL  NULL

