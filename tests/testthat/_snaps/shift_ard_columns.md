# rename_ard_columns() works

    Code
      as.data.frame(dplyr::slice(dplyr::select(res_rnm_var, -c(fmt_fn, warning, error)), 1:20))
    Output
             AGE   BMIBL HEIGHTBL group1 group1_level    context stat_name stat_label     stat
      1  Overall    <NA>     <NA> TRT01A      Placebo continuous         N          N       86
      2  Overall    <NA>     <NA> TRT01A      Placebo continuous      mean       Mean  75.2093
      3  Overall    <NA>     <NA> TRT01A      Placebo continuous        sd         SD 8.590167
      4  Overall    <NA>     <NA> TRT01A      Placebo continuous    median     Median       76
      5  Overall    <NA>     <NA> TRT01A      Placebo continuous       p25         Q1       69
      6  Overall    <NA>     <NA> TRT01A      Placebo continuous       p75         Q3       82
      7  Overall    <NA>     <NA> TRT01A      Placebo continuous       min        Min       52
      8  Overall    <NA>     <NA> TRT01A      Placebo continuous       max        Max       89
      9     <NA> Overall     <NA> TRT01A      Placebo continuous         N          N       86
      10    <NA> Overall     <NA> TRT01A      Placebo continuous      mean       Mean 23.63605
      11    <NA> Overall     <NA> TRT01A      Placebo continuous        sd         SD 3.671926
      12    <NA> Overall     <NA> TRT01A      Placebo continuous    median     Median     23.4
      13    <NA> Overall     <NA> TRT01A      Placebo continuous       p25         Q1     21.2
      14    <NA> Overall     <NA> TRT01A      Placebo continuous       p75         Q3     25.6
      15    <NA> Overall     <NA> TRT01A      Placebo continuous       min        Min     15.1
      16    <NA> Overall     <NA> TRT01A      Placebo continuous       max        Max     33.3
      17    <NA>    <NA>  Overall TRT01A      Placebo continuous         N          N       86
      18    <NA>    <NA>  Overall TRT01A      Placebo continuous      mean       Mean 162.5733
      19    <NA>    <NA>  Overall TRT01A      Placebo continuous        sd         SD 11.52236
      20    <NA>    <NA>  Overall TRT01A      Placebo continuous    median     Median    162.6

---

    Code
      as.data.frame(dplyr::slice(dplyr::select(res_multi_1, -c(fmt_fn, warning, error)), 1:20))
    Output
          TRT01A SEX                             RACE                 ETHNIC     AGE     context stat_name stat_label       stat group1_level group2_level
      1  Placebo   F AMERICAN INDIAN OR ALASKA NATIVE                   <NA>    <NA> categorical         n          n          0         NULL         NULL
      2  Placebo   F AMERICAN INDIAN OR ALASKA NATIVE                   <NA>    <NA> categorical         N          N         53         NULL         NULL
      3  Placebo   F AMERICAN INDIAN OR ALASKA NATIVE                   <NA>    <NA> categorical         p          %          0         NULL         NULL
      4  Placebo   F        BLACK OR AFRICAN AMERICAN                   <NA>    <NA> categorical         n          n          5         NULL         NULL
      5  Placebo   F        BLACK OR AFRICAN AMERICAN                   <NA>    <NA> categorical         N          N         53         NULL         NULL
      6  Placebo   F        BLACK OR AFRICAN AMERICAN                   <NA>    <NA> categorical         p          % 0.09433962         NULL         NULL
      7  Placebo   F                            WHITE                   <NA>    <NA> categorical         n          n         48         NULL         NULL
      8  Placebo   F                            WHITE                   <NA>    <NA> categorical         N          N         53         NULL         NULL
      9  Placebo   F                            WHITE                   <NA>    <NA> categorical         p          %  0.9056604         NULL         NULL
      10 Placebo   F                             <NA>     HISPANIC OR LATINO    <NA> categorical         n          n          2         NULL         NULL
      11 Placebo   F                             <NA>     HISPANIC OR LATINO    <NA> categorical         N          N         53         NULL         NULL
      12 Placebo   F                             <NA>     HISPANIC OR LATINO    <NA> categorical         p          % 0.03773585         NULL         NULL
      13 Placebo   F                             <NA> NOT HISPANIC OR LATINO    <NA> categorical         n          n         51         NULL         NULL
      14 Placebo   F                             <NA> NOT HISPANIC OR LATINO    <NA> categorical         N          N         53         NULL         NULL
      15 Placebo   F                             <NA> NOT HISPANIC OR LATINO    <NA> categorical         p          %  0.9622642         NULL         NULL
      16 Placebo   F                             <NA>                   <NA> Overall  continuous         N          N         53         NULL         NULL
      17 Placebo   F                             <NA>                   <NA> Overall  continuous      mean       Mean   76.35849         NULL         NULL
      18 Placebo   F                             <NA>                   <NA> Overall  continuous        sd         SD   8.733433         NULL         NULL
      19 Placebo   F                             <NA>                   <NA> Overall  continuous    median     Median         78         NULL         NULL
      20 Placebo   F                             <NA>                   <NA> Overall  continuous       p25         Q1         70         NULL         NULL

