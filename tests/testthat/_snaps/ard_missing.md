# ard_missing() works

    Code
      as.data.frame(dplyr::select(ard, -"statistic_fmt_fn"))
    Output
         group1         group1_level variable context stat_name    stat_label
      1     ARM              Placebo    BMIBL missing     N_obs Vector Length
      2     ARM              Placebo    BMIBL missing    N_miss     N Missing
      3     ARM              Placebo    BMIBL missing N_nonmiss N Non-missing
      4     ARM              Placebo    BMIBL missing    p_miss     % Missing
      5     ARM              Placebo    BMIBL missing p_nonmiss % Non-missing
      6     ARM Xanomeline High Dose    BMIBL missing     N_obs Vector Length
      7     ARM Xanomeline High Dose    BMIBL missing    N_miss     N Missing
      8     ARM Xanomeline High Dose    BMIBL missing N_nonmiss N Non-missing
      9     ARM Xanomeline High Dose    BMIBL missing    p_miss     % Missing
      10    ARM Xanomeline High Dose    BMIBL missing p_nonmiss % Non-missing
      11    ARM  Xanomeline Low Dose    BMIBL missing     N_obs Vector Length
      12    ARM  Xanomeline Low Dose    BMIBL missing    N_miss     N Missing
      13    ARM  Xanomeline Low Dose    BMIBL missing N_nonmiss N Non-missing
      14    ARM  Xanomeline Low Dose    BMIBL missing    p_miss     % Missing
      15    ARM  Xanomeline Low Dose    BMIBL missing p_nonmiss % Non-missing
          statistic warning error
      1          86    NULL  NULL
      2           0    NULL  NULL
      3          86    NULL  NULL
      4           0    NULL  NULL
      5           1    NULL  NULL
      6          84    NULL  NULL
      7           0    NULL  NULL
      8          84    NULL  NULL
      9           0    NULL  NULL
      10          1    NULL  NULL
      11         84    NULL  NULL
      12          1    NULL  NULL
      13         83    NULL  NULL
      14 0.01190476    NULL  NULL
      15  0.9880952    NULL  NULL

