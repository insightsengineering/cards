# rename_ard_columns() works

    Code
      as.data.frame(dplyr::slice(dplyr::select(res_rnm_var, -c(fmt_fn, warning, error)),
      1:20))
    Output
         group1         group1_level         AGE BMIBL HEIGHTBL    context stat_name
      1  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous         N
      2  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous      mean
      3  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous        sd
      4  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous    median
      5  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous       p25
      6  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous       p75
      7  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous       min
      8  TRT01A              Placebo Overall AGE  <NA>     <NA> continuous       max
      9  TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous         N
      10 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous      mean
      11 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous        sd
      12 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous    median
      13 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous       p25
      14 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous       p75
      15 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous       min
      16 TRT01A Xanomeline High Dose Overall AGE  <NA>     <NA> continuous       max
      17 TRT01A  Xanomeline Low Dose Overall AGE  <NA>     <NA> continuous         N
      18 TRT01A  Xanomeline Low Dose Overall AGE  <NA>     <NA> continuous      mean
      19 TRT01A  Xanomeline Low Dose Overall AGE  <NA>     <NA> continuous        sd
      20 TRT01A  Xanomeline Low Dose Overall AGE  <NA>     <NA> continuous    median
         stat_label     stat
      1           N       86
      2        Mean  75.2093
      3          SD 8.590167
      4      Median       76
      5          Q1       69
      6          Q3       82
      7         Min       52
      8         Max       89
      9           N       84
      10       Mean 74.38095
      11         SD 7.886094
      12     Median       76
      13         Q1     70.5
      14         Q3       80
      15        Min       56
      16        Max       88
      17          N       84
      18       Mean 75.66667
      19         SD 8.286051
      20     Median     77.5

---

    Code
      as.data.frame(dplyr::slice(dplyr::select(res_multi_1, -c(fmt_fn, warning, error)),
      1:20))
    Output
                       TRT01A SEX                             RACE ETHNIC  AGE
      1               Placebo   F AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      2               Placebo   F AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      3               Placebo   F AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      4               Placebo   F        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      5               Placebo   F        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      6               Placebo   F        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      7               Placebo   F                            WHITE   <NA> <NA>
      8               Placebo   F                            WHITE   <NA> <NA>
      9               Placebo   F                            WHITE   <NA> <NA>
      10              Placebo   M AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      11              Placebo   M AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      12              Placebo   M AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      13              Placebo   M        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      14              Placebo   M        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      15              Placebo   M        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      16              Placebo   M                            WHITE   <NA> <NA>
      17              Placebo   M                            WHITE   <NA> <NA>
      18              Placebo   M                            WHITE   <NA> <NA>
      19 Xanomeline High Dose   F AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      20 Xanomeline High Dose   F AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
             context stat_name stat_label       stat
      1  categorical         n          n          0
      2  categorical         N          N         53
      3  categorical         p          %          0
      4  categorical         n          n          5
      5  categorical         N          N         53
      6  categorical         p          % 0.09433962
      7  categorical         n          n         48
      8  categorical         N          N         53
      9  categorical         p          %  0.9056604
      10 categorical         n          n          0
      11 categorical         N          N         33
      12 categorical         p          %          0
      13 categorical         n          n          3
      14 categorical         N          N         33
      15 categorical         p          % 0.09090909
      16 categorical         n          n         30
      17 categorical         N          N         33
      18 categorical         p          %  0.9090909
      19 categorical         n          n          0
      20 categorical         N          N         40

---

    Code
      as.data.frame(dplyr::slice(rename_ard_columns(res_shuffle, c(variable, label)),
      1:20))
    Output
                       TRT01A                             RACE ETHNIC  AGE
      1               Placebo AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      2               Placebo AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      3               Placebo AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      4               Placebo        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      5               Placebo        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      6               Placebo        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      7               Placebo                            WHITE   <NA> <NA>
      8               Placebo                            WHITE   <NA> <NA>
      9               Placebo                            WHITE   <NA> <NA>
      10 Xanomeline High Dose AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      11 Xanomeline High Dose AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      12 Xanomeline High Dose AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      13 Xanomeline High Dose        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      14 Xanomeline High Dose        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      15 Xanomeline High Dose        BLACK OR AFRICAN AMERICAN   <NA> <NA>
      16 Xanomeline High Dose                            WHITE   <NA> <NA>
      17 Xanomeline High Dose                            WHITE   <NA> <NA>
      18 Xanomeline High Dose                            WHITE   <NA> <NA>
      19  Xanomeline Low Dose AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
      20  Xanomeline Low Dose AMERICAN INDIAN OR ALASKA NATIVE   <NA> <NA>
             context stat_name        stat
      1  categorical         n  0.00000000
      2  categorical         N 86.00000000
      3  categorical         p  0.00000000
      4  categorical         n  8.00000000
      5  categorical         N 86.00000000
      6  categorical         p  0.09302326
      7  categorical         n 78.00000000
      8  categorical         N 86.00000000
      9  categorical         p  0.90697674
      10 categorical         n  1.00000000
      11 categorical         N 84.00000000
      12 categorical         p  0.01190476
      13 categorical         n  9.00000000
      14 categorical         N 84.00000000
      15 categorical         p  0.10714286
      16 categorical         n 74.00000000
      17 categorical         N 84.00000000
      18 categorical         p  0.88095238
      19 categorical         n  0.00000000
      20 categorical         N 84.00000000

---

    Code
      rename_ard_columns(dplyr::select(res_var, -group1), "group1_level")
    Message
      ! The following `*_level` columns do not have a match and will not be renamed: "group1_level"
      {cards} data frame: 72 x 9
    Output
         group1_level variable   context stat_name stat_label   stat
      1       Placebo      AGE continuo…         N          N     86
      2       Placebo      AGE continuo…      mean       Mean 75.209
      3       Placebo      AGE continuo…        sd         SD   8.59
      4       Placebo      AGE continuo…    median     Median     76
      5       Placebo      AGE continuo…       p25         Q1     69
      6       Placebo      AGE continuo…       p75         Q3     82
      7       Placebo      AGE continuo…       min        Min     52
      8       Placebo      AGE continuo…       max        Max     89
      9       Placebo    BMIBL continuo…         N          N     86
      10      Placebo    BMIBL continuo…      mean       Mean 23.636
    Message
      i 62 more rows
      i Use `print(n = ...)` to see more rows
      i 3 more variables: fmt_fn, warning, error

