# ard_effectsize_hedges_g() works

    Code
      as.data.frame(dplyr::slice_head(dplyr::group_by(dplyr::select(
        ard_effectsize_hedges_g(dplyr::filter(cards::ADSL, ARM %in% c("Placebo",
          "Xanomeline High Dose")), by = ARM, variables = c(BMIBL, HEIGHTBL)), c(1:3,
        5:6)), variable), n = 3))
    Output
        group1 variable             context           stat_label       stat
      1    ARM    BMIBL effectsize_hedges_g Effect Size Estimate -0.4347006
      2    ARM    BMIBL effectsize_hedges_g  CI Confidence Level       0.95
      3    ARM    BMIBL effectsize_hedges_g       CI Lower Bound -0.7369717
      4    ARM HEIGHTBL effectsize_hedges_g Effect Size Estimate -0.2977188
      5    ARM HEIGHTBL effectsize_hedges_g  CI Confidence Level       0.95
      6    ARM HEIGHTBL effectsize_hedges_g       CI Lower Bound -0.5982873

