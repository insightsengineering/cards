# ard_survey_svyranktest() works

    Code
      dplyr::select(as.data.frame(svyranktest[[1]]), stat_label, stat)
    Output
                      stat_label                            stat
      1 Median of the Difference                      -0.1060602
      2                Statistic                       -1.718689
      3                  p-value                      0.09426084
      4       Degrees of Freedom                              36
      5                   method Design-based KruskalWallis test
      6   Alternative Hypothesis                       two.sided

---

    Code
      dplyr::select(as.data.frame(svyranktest[[2]]), stat_label, stat)
    Output
                      stat_label                            stat
      1 Median of the Difference                      -0.3791163
      2                Statistic                       -1.583859
      3                  p-value                       0.1219723
      4       Degrees of Freedom                              36
      5                   method Design-based vanderWaerden test
      6   Alternative Hypothesis                       two.sided

---

    Code
      dplyr::select(as.data.frame(svyranktest[[3]]), stat_label, stat)
    Output
                      stat_label                     stat
      1 Median of the Difference               -0.1240709
      2                Statistic               -0.9139828
      3                  p-value                0.3668071
      4       Degrees of Freedom                       36
      5                   method Design-based median test
      6   Alternative Hypothesis                two.sided

---

    Code
      dplyr::select(as.data.frame(svyranktest[[4]]), stat_label, stat)
    Output
                      stat_label                            stat
      1 Median of the Difference                      -0.1060602
      2                Statistic                       -1.718689
      3                  p-value                      0.09426084
      4       Degrees of Freedom                              36
      5                   method Design-based KruskalWallis test
      6   Alternative Hypothesis                       two.sided

