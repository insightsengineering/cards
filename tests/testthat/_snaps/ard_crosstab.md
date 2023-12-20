# ard_crosstab(percent='row') works

    Code
      as.data.frame(dplyr::select(flatten_ard(apply_statistic_fmt_fn(ard_with_args)),
      -statistic_fmt_fn, -warning, -error))
    Output
         group1         group1_level variable variable_level stat_name stat_label
      1     ARM              Placebo   AGEGR1          65-80         n          n
      2     ARM              Placebo   AGEGR1            <65         n          n
      3     ARM              Placebo   AGEGR1            >80         n          n
      4     ARM              Placebo   AGEGR1          65-80         N          N
      5     ARM              Placebo   AGEGR1            <65         N          N
      6     ARM              Placebo   AGEGR1            >80         N          N
      7     ARM Xanomeline High Dose   AGEGR1          65-80         n          n
      8     ARM Xanomeline High Dose   AGEGR1            <65         n          n
      9     ARM Xanomeline High Dose   AGEGR1            >80         n          n
      10    ARM Xanomeline High Dose   AGEGR1          65-80         N          N
      11    ARM Xanomeline High Dose   AGEGR1            <65         N          N
      12    ARM Xanomeline High Dose   AGEGR1            >80         N          N
      13    ARM  Xanomeline Low Dose   AGEGR1          65-80         n          n
      14    ARM  Xanomeline Low Dose   AGEGR1            <65         n          n
      15    ARM  Xanomeline Low Dose   AGEGR1            >80         n          n
      16    ARM  Xanomeline Low Dose   AGEGR1          65-80         N          N
      17    ARM  Xanomeline Low Dose   AGEGR1            <65         N          N
      18    ARM  Xanomeline Low Dose   AGEGR1            >80         N          N
         statistic statistic_fmt
      1         42         42.00
      2         14         14.00
      3         30         30.00
      4        144           144
      5         33            33
      6         77            77
      7         55         55.00
      8         11         11.00
      9         18         18.00
      10       144           144
      11        33            33
      12        77            77
      13        47         47.00
      14         8          8.00
      15        29         29.00
      16       144           144
      17        33            33
      18        77            77

