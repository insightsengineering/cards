# ard_effectsize_cohens_d() works

    Code
      as.data.frame(dplyr::slice_head(dplyr::group_by(dplyr::select(
        ard_effectsize_cohens_d(dplyr::filter(cards::ADSL, ARM %in% c("Placebo",
          "Xanomeline High Dose")), by = ARM, variables = c(BMIBL, HEIGHTBL)), c(1:3,
        5:6)), variable), n = 3))
    Output
        group1 variable             context           stat_label      stat
      1    ARM    BMIBL effectsize_cohens_d Effect Size Estimate -0.436653
      2    ARM    BMIBL effectsize_cohens_d  CI Confidence Level      0.95
      3    ARM    BMIBL effectsize_cohens_d       CI Lower Bound -0.740282
      4    ARM HEIGHTBL effectsize_cohens_d Effect Size Estimate -0.299056
      5    ARM HEIGHTBL effectsize_cohens_d  CI Confidence Level      0.95
      6    ARM HEIGHTBL effectsize_cohens_d       CI Lower Bound -0.600975

