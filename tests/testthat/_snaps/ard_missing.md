# ard_missing() works

    Code
      as.data.frame(dplyr::select(ard, -"fmt_fn"))
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
               stat warning error
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

# ard_missing(stat_label) argument works

    Code
      unique(dplyr::filter(dplyr::select(as.data.frame(ard_missing(data = ADSL, by = "ARM",
        variables = c("AGE", "BMIBL"), stat_label = everything() ~ list(c("N_obs",
          "N_miss") ~ "N, miss"))), stat_name, stat_label), stat_name %in% c("N_obs",
        "N_miss")))
    Output
        stat_name stat_label
      1     N_obs    N, miss
      2    N_miss    N, miss

---

    Code
      unique(dplyr::filter(dplyr::select(as.data.frame(ard_missing(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_label = everything() ~ list(p_miss = "% miss",
          p_nonmiss = "% non miss"))), stat_name, stat_label), stat_name %in% c(
        "p_miss", "p_nonmiss")))
    Output
        stat_name stat_label
      1    p_miss     % miss
      2 p_nonmiss % non miss

---

    Code
      unique(dplyr::filter(dplyr::select(as.data.frame(ard_missing(data = ADSL, by = "ARM",
        variables = c("AGE", "BMIBL"), stat_label = AGE ~ list(N_obs = "Number of Obs"))),
      variable, stat_name, stat_label), stat_name == "N_obs"))
    Output
        variable stat_name    stat_label
      1      AGE     N_obs Number of Obs
      2    BMIBL     N_obs Vector Length

