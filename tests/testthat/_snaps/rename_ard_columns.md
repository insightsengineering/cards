# rename_ard_columns() works

    Code
      as.data.frame(dplyr::select(res_rnm_var, -c(fmt_fn, warning, error)))
    Output
         group1         group1_level         AGE         BMIBL         HEIGHTBL
      1  TRT01A              Placebo Overall AGE          <NA>             <NA>
      2  TRT01A              Placebo Overall AGE          <NA>             <NA>
      3  TRT01A              Placebo Overall AGE          <NA>             <NA>
      4  TRT01A              Placebo Overall AGE          <NA>             <NA>
      5  TRT01A              Placebo Overall AGE          <NA>             <NA>
      6  TRT01A              Placebo Overall AGE          <NA>             <NA>
      7  TRT01A              Placebo Overall AGE          <NA>             <NA>
      8  TRT01A              Placebo Overall AGE          <NA>             <NA>
      9  TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      10 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      11 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      12 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      13 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      14 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      15 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      16 TRT01A Xanomeline High Dose Overall AGE          <NA>             <NA>
      17 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      18 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      19 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      20 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      21 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      22 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      23 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      24 TRT01A  Xanomeline Low Dose Overall AGE          <NA>             <NA>
      25 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      26 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      27 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      28 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      29 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      30 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      31 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      32 TRT01A              Placebo        <NA> Overall BMIBL             <NA>
      33 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      34 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      35 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      36 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      37 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      38 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      39 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      40 TRT01A Xanomeline High Dose        <NA> Overall BMIBL             <NA>
      41 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      42 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      43 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      44 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      45 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      46 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      47 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      48 TRT01A  Xanomeline Low Dose        <NA> Overall BMIBL             <NA>
      49 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      50 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      51 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      52 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      53 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      54 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      55 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      56 TRT01A              Placebo        <NA>          <NA> Overall HEIGHTBL
      57 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      58 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      59 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      60 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      61 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      62 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      63 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      64 TRT01A Xanomeline High Dose        <NA>          <NA> Overall HEIGHTBL
      65 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      66 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      67 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      68 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      69 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      70 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      71 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
      72 TRT01A  Xanomeline Low Dose        <NA>          <NA> Overall HEIGHTBL
            context stat_name      stat_label     stat
      1  continuous         N               N       86
      2  continuous      mean            Mean  75.2093
      3  continuous        sd              SD 8.590167
      4  continuous    median          Median       76
      5  continuous       p25 25th Percentile       69
      6  continuous       p75 75th Percentile       82
      7  continuous       min             Min       52
      8  continuous       max             Max       89
      9  continuous         N               N       84
      10 continuous      mean            Mean 74.38095
      11 continuous        sd              SD 7.886094
      12 continuous    median          Median       76
      13 continuous       p25 25th Percentile     70.5
      14 continuous       p75 75th Percentile       80
      15 continuous       min             Min       56
      16 continuous       max             Max       88
      17 continuous         N               N       84
      18 continuous      mean            Mean 75.66667
      19 continuous        sd              SD 8.286051
      20 continuous    median          Median     77.5
      21 continuous       p25 25th Percentile       71
      22 continuous       p75 75th Percentile       82
      23 continuous       min             Min       51
      24 continuous       max             Max       88
      25 continuous         N               N       86
      26 continuous      mean            Mean 23.63605
      27 continuous        sd              SD 3.671926
      28 continuous    median          Median     23.4
      29 continuous       p25 25th Percentile     21.2
      30 continuous       p75 75th Percentile     25.6
      31 continuous       min             Min     15.1
      32 continuous       max             Max     33.3
      33 continuous         N               N       84
      34 continuous      mean            Mean 25.34762
      35 continuous        sd              SD 4.158269
      36 continuous    median          Median     24.8
      37 continuous       p25 25th Percentile     22.7
      38 continuous       p75 75th Percentile     27.9
      39 continuous       min             Min     13.7
      40 continuous       max             Max     34.5
      41 continuous         N               N       83
      42 continuous      mean            Mean 25.06265
      43 continuous        sd              SD 4.270509
      44 continuous    median          Median     24.3
      45 continuous       p25 25th Percentile     22.1
      46 continuous       p75 75th Percentile     27.8
      47 continuous       min             Min     17.7
      48 continuous       max             Max     40.1
      49 continuous         N               N       86
      50 continuous      mean            Mean 162.5733
      51 continuous        sd              SD 11.52236
      52 continuous    median          Median    162.6
      53 continuous       p25 25th Percentile    153.7
      54 continuous       p75 75th Percentile    171.5
      55 continuous       min             Min    137.2
      56 continuous       max             Max    185.4
      57 continuous         N               N       84
      58 continuous      mean            Mean 165.8202
      59 continuous        sd              SD 10.13135
      60 continuous    median          Median    165.1
      61 continuous       p25 25th Percentile    157.5
      62 continuous       p75 75th Percentile   172.85
      63 continuous       min             Min    146.1
      64 continuous       max             Max    190.5
      65 continuous         N               N       84
      66 continuous      mean            Mean 163.4333
      67 continuous        sd              SD 10.41924
      68 continuous    median          Median    162.6
      69 continuous       p25 25th Percentile    157.5
      70 continuous       p75 75th Percentile    170.2
      71 continuous       min             Min    135.9
      72 continuous       max             Max    195.6

---

    Code
      as.data.frame(dplyr::select(res_multi_1, -c(fmt_fn, warning, error)))
    Output
                        TRT01A  SEX                             RACE
      1                Placebo    F AMERICAN INDIAN OR ALASKA NATIVE
      2                Placebo    F AMERICAN INDIAN OR ALASKA NATIVE
      3                Placebo    F AMERICAN INDIAN OR ALASKA NATIVE
      4                Placebo    F        BLACK OR AFRICAN AMERICAN
      5                Placebo    F        BLACK OR AFRICAN AMERICAN
      6                Placebo    F        BLACK OR AFRICAN AMERICAN
      7                Placebo    F                            WHITE
      8                Placebo    F                            WHITE
      9                Placebo    F                            WHITE
      10               Placebo    M AMERICAN INDIAN OR ALASKA NATIVE
      11               Placebo    M AMERICAN INDIAN OR ALASKA NATIVE
      12               Placebo    M AMERICAN INDIAN OR ALASKA NATIVE
      13               Placebo    M        BLACK OR AFRICAN AMERICAN
      14               Placebo    M        BLACK OR AFRICAN AMERICAN
      15               Placebo    M        BLACK OR AFRICAN AMERICAN
      16               Placebo    M                            WHITE
      17               Placebo    M                            WHITE
      18               Placebo    M                            WHITE
      19  Xanomeline High Dose    F AMERICAN INDIAN OR ALASKA NATIVE
      20  Xanomeline High Dose    F AMERICAN INDIAN OR ALASKA NATIVE
      21  Xanomeline High Dose    F AMERICAN INDIAN OR ALASKA NATIVE
      22  Xanomeline High Dose    F        BLACK OR AFRICAN AMERICAN
      23  Xanomeline High Dose    F        BLACK OR AFRICAN AMERICAN
      24  Xanomeline High Dose    F        BLACK OR AFRICAN AMERICAN
      25  Xanomeline High Dose    F                            WHITE
      26  Xanomeline High Dose    F                            WHITE
      27  Xanomeline High Dose    F                            WHITE
      28  Xanomeline High Dose    M AMERICAN INDIAN OR ALASKA NATIVE
      29  Xanomeline High Dose    M AMERICAN INDIAN OR ALASKA NATIVE
      30  Xanomeline High Dose    M AMERICAN INDIAN OR ALASKA NATIVE
      31  Xanomeline High Dose    M        BLACK OR AFRICAN AMERICAN
      32  Xanomeline High Dose    M        BLACK OR AFRICAN AMERICAN
      33  Xanomeline High Dose    M        BLACK OR AFRICAN AMERICAN
      34  Xanomeline High Dose    M                            WHITE
      35  Xanomeline High Dose    M                            WHITE
      36  Xanomeline High Dose    M                            WHITE
      37   Xanomeline Low Dose    F AMERICAN INDIAN OR ALASKA NATIVE
      38   Xanomeline Low Dose    F AMERICAN INDIAN OR ALASKA NATIVE
      39   Xanomeline Low Dose    F AMERICAN INDIAN OR ALASKA NATIVE
      40   Xanomeline Low Dose    F        BLACK OR AFRICAN AMERICAN
      41   Xanomeline Low Dose    F        BLACK OR AFRICAN AMERICAN
      42   Xanomeline Low Dose    F        BLACK OR AFRICAN AMERICAN
      43   Xanomeline Low Dose    F                            WHITE
      44   Xanomeline Low Dose    F                            WHITE
      45   Xanomeline Low Dose    F                            WHITE
      46   Xanomeline Low Dose    M AMERICAN INDIAN OR ALASKA NATIVE
      47   Xanomeline Low Dose    M AMERICAN INDIAN OR ALASKA NATIVE
      48   Xanomeline Low Dose    M AMERICAN INDIAN OR ALASKA NATIVE
      49   Xanomeline Low Dose    M        BLACK OR AFRICAN AMERICAN
      50   Xanomeline Low Dose    M        BLACK OR AFRICAN AMERICAN
      51   Xanomeline Low Dose    M        BLACK OR AFRICAN AMERICAN
      52   Xanomeline Low Dose    M                            WHITE
      53   Xanomeline Low Dose    M                            WHITE
      54   Xanomeline Low Dose    M                            WHITE
      55               Placebo    F                             <NA>
      56               Placebo    F                             <NA>
      57               Placebo    F                             <NA>
      58               Placebo    F                             <NA>
      59               Placebo    F                             <NA>
      60               Placebo    F                             <NA>
      61               Placebo    M                             <NA>
      62               Placebo    M                             <NA>
      63               Placebo    M                             <NA>
      64               Placebo    M                             <NA>
      65               Placebo    M                             <NA>
      66               Placebo    M                             <NA>
      67  Xanomeline High Dose    F                             <NA>
      68  Xanomeline High Dose    F                             <NA>
      69  Xanomeline High Dose    F                             <NA>
      70  Xanomeline High Dose    F                             <NA>
      71  Xanomeline High Dose    F                             <NA>
      72  Xanomeline High Dose    F                             <NA>
      73  Xanomeline High Dose    M                             <NA>
      74  Xanomeline High Dose    M                             <NA>
      75  Xanomeline High Dose    M                             <NA>
      76  Xanomeline High Dose    M                             <NA>
      77  Xanomeline High Dose    M                             <NA>
      78  Xanomeline High Dose    M                             <NA>
      79   Xanomeline Low Dose    F                             <NA>
      80   Xanomeline Low Dose    F                             <NA>
      81   Xanomeline Low Dose    F                             <NA>
      82   Xanomeline Low Dose    F                             <NA>
      83   Xanomeline Low Dose    F                             <NA>
      84   Xanomeline Low Dose    F                             <NA>
      85   Xanomeline Low Dose    M                             <NA>
      86   Xanomeline Low Dose    M                             <NA>
      87   Xanomeline Low Dose    M                             <NA>
      88   Xanomeline Low Dose    M                             <NA>
      89   Xanomeline Low Dose    M                             <NA>
      90   Xanomeline Low Dose    M                             <NA>
      91               Placebo    F                             <NA>
      92               Placebo    F                             <NA>
      93               Placebo    F                             <NA>
      94               Placebo    F                             <NA>
      95               Placebo    F                             <NA>
      96               Placebo    F                             <NA>
      97               Placebo    F                             <NA>
      98               Placebo    F                             <NA>
      99               Placebo    M                             <NA>
      100              Placebo    M                             <NA>
      101              Placebo    M                             <NA>
      102              Placebo    M                             <NA>
      103              Placebo    M                             <NA>
      104              Placebo    M                             <NA>
      105              Placebo    M                             <NA>
      106              Placebo    M                             <NA>
      107 Xanomeline High Dose    F                             <NA>
      108 Xanomeline High Dose    F                             <NA>
      109 Xanomeline High Dose    F                             <NA>
      110 Xanomeline High Dose    F                             <NA>
      111 Xanomeline High Dose    F                             <NA>
      112 Xanomeline High Dose    F                             <NA>
      113 Xanomeline High Dose    F                             <NA>
      114 Xanomeline High Dose    F                             <NA>
      115 Xanomeline High Dose    M                             <NA>
      116 Xanomeline High Dose    M                             <NA>
      117 Xanomeline High Dose    M                             <NA>
      118 Xanomeline High Dose    M                             <NA>
      119 Xanomeline High Dose    M                             <NA>
      120 Xanomeline High Dose    M                             <NA>
      121 Xanomeline High Dose    M                             <NA>
      122 Xanomeline High Dose    M                             <NA>
      123  Xanomeline Low Dose    F                             <NA>
      124  Xanomeline Low Dose    F                             <NA>
      125  Xanomeline Low Dose    F                             <NA>
      126  Xanomeline Low Dose    F                             <NA>
      127  Xanomeline Low Dose    F                             <NA>
      128  Xanomeline Low Dose    F                             <NA>
      129  Xanomeline Low Dose    F                             <NA>
      130  Xanomeline Low Dose    F                             <NA>
      131  Xanomeline Low Dose    M                             <NA>
      132  Xanomeline Low Dose    M                             <NA>
      133  Xanomeline Low Dose    M                             <NA>
      134  Xanomeline Low Dose    M                             <NA>
      135  Xanomeline Low Dose    M                             <NA>
      136  Xanomeline Low Dose    M                             <NA>
      137  Xanomeline Low Dose    M                             <NA>
      138  Xanomeline Low Dose    M                             <NA>
      139              Placebo <NA>                             <NA>
      140              Placebo <NA>                             <NA>
      141              Placebo <NA>                             <NA>
      142 Xanomeline High Dose <NA>                             <NA>
      143 Xanomeline High Dose <NA>                             <NA>
      144 Xanomeline High Dose <NA>                             <NA>
      145  Xanomeline Low Dose <NA>                             <NA>
      146  Xanomeline Low Dose <NA>                             <NA>
      147  Xanomeline Low Dose <NA>                             <NA>
      148                 <NA>    F                             <NA>
      149                 <NA>    F                             <NA>
      150                 <NA>    F                             <NA>
      151                 <NA>    M                             <NA>
      152                 <NA>    M                             <NA>
      153                 <NA>    M                             <NA>
                          ETHNIC         AGE     context stat_name      stat_label
      1                     <NA>        <NA> categorical         n               n
      2                     <NA>        <NA> categorical         N               N
      3                     <NA>        <NA> categorical         p               %
      4                     <NA>        <NA> categorical         n               n
      5                     <NA>        <NA> categorical         N               N
      6                     <NA>        <NA> categorical         p               %
      7                     <NA>        <NA> categorical         n               n
      8                     <NA>        <NA> categorical         N               N
      9                     <NA>        <NA> categorical         p               %
      10                    <NA>        <NA> categorical         n               n
      11                    <NA>        <NA> categorical         N               N
      12                    <NA>        <NA> categorical         p               %
      13                    <NA>        <NA> categorical         n               n
      14                    <NA>        <NA> categorical         N               N
      15                    <NA>        <NA> categorical         p               %
      16                    <NA>        <NA> categorical         n               n
      17                    <NA>        <NA> categorical         N               N
      18                    <NA>        <NA> categorical         p               %
      19                    <NA>        <NA> categorical         n               n
      20                    <NA>        <NA> categorical         N               N
      21                    <NA>        <NA> categorical         p               %
      22                    <NA>        <NA> categorical         n               n
      23                    <NA>        <NA> categorical         N               N
      24                    <NA>        <NA> categorical         p               %
      25                    <NA>        <NA> categorical         n               n
      26                    <NA>        <NA> categorical         N               N
      27                    <NA>        <NA> categorical         p               %
      28                    <NA>        <NA> categorical         n               n
      29                    <NA>        <NA> categorical         N               N
      30                    <NA>        <NA> categorical         p               %
      31                    <NA>        <NA> categorical         n               n
      32                    <NA>        <NA> categorical         N               N
      33                    <NA>        <NA> categorical         p               %
      34                    <NA>        <NA> categorical         n               n
      35                    <NA>        <NA> categorical         N               N
      36                    <NA>        <NA> categorical         p               %
      37                    <NA>        <NA> categorical         n               n
      38                    <NA>        <NA> categorical         N               N
      39                    <NA>        <NA> categorical         p               %
      40                    <NA>        <NA> categorical         n               n
      41                    <NA>        <NA> categorical         N               N
      42                    <NA>        <NA> categorical         p               %
      43                    <NA>        <NA> categorical         n               n
      44                    <NA>        <NA> categorical         N               N
      45                    <NA>        <NA> categorical         p               %
      46                    <NA>        <NA> categorical         n               n
      47                    <NA>        <NA> categorical         N               N
      48                    <NA>        <NA> categorical         p               %
      49                    <NA>        <NA> categorical         n               n
      50                    <NA>        <NA> categorical         N               N
      51                    <NA>        <NA> categorical         p               %
      52                    <NA>        <NA> categorical         n               n
      53                    <NA>        <NA> categorical         N               N
      54                    <NA>        <NA> categorical         p               %
      55      HISPANIC OR LATINO        <NA> categorical         n               n
      56      HISPANIC OR LATINO        <NA> categorical         N               N
      57      HISPANIC OR LATINO        <NA> categorical         p               %
      58  NOT HISPANIC OR LATINO        <NA> categorical         n               n
      59  NOT HISPANIC OR LATINO        <NA> categorical         N               N
      60  NOT HISPANIC OR LATINO        <NA> categorical         p               %
      61      HISPANIC OR LATINO        <NA> categorical         n               n
      62      HISPANIC OR LATINO        <NA> categorical         N               N
      63      HISPANIC OR LATINO        <NA> categorical         p               %
      64  NOT HISPANIC OR LATINO        <NA> categorical         n               n
      65  NOT HISPANIC OR LATINO        <NA> categorical         N               N
      66  NOT HISPANIC OR LATINO        <NA> categorical         p               %
      67      HISPANIC OR LATINO        <NA> categorical         n               n
      68      HISPANIC OR LATINO        <NA> categorical         N               N
      69      HISPANIC OR LATINO        <NA> categorical         p               %
      70  NOT HISPANIC OR LATINO        <NA> categorical         n               n
      71  NOT HISPANIC OR LATINO        <NA> categorical         N               N
      72  NOT HISPANIC OR LATINO        <NA> categorical         p               %
      73      HISPANIC OR LATINO        <NA> categorical         n               n
      74      HISPANIC OR LATINO        <NA> categorical         N               N
      75      HISPANIC OR LATINO        <NA> categorical         p               %
      76  NOT HISPANIC OR LATINO        <NA> categorical         n               n
      77  NOT HISPANIC OR LATINO        <NA> categorical         N               N
      78  NOT HISPANIC OR LATINO        <NA> categorical         p               %
      79      HISPANIC OR LATINO        <NA> categorical         n               n
      80      HISPANIC OR LATINO        <NA> categorical         N               N
      81      HISPANIC OR LATINO        <NA> categorical         p               %
      82  NOT HISPANIC OR LATINO        <NA> categorical         n               n
      83  NOT HISPANIC OR LATINO        <NA> categorical         N               N
      84  NOT HISPANIC OR LATINO        <NA> categorical         p               %
      85      HISPANIC OR LATINO        <NA> categorical         n               n
      86      HISPANIC OR LATINO        <NA> categorical         N               N
      87      HISPANIC OR LATINO        <NA> categorical         p               %
      88  NOT HISPANIC OR LATINO        <NA> categorical         n               n
      89  NOT HISPANIC OR LATINO        <NA> categorical         N               N
      90  NOT HISPANIC OR LATINO        <NA> categorical         p               %
      91                    <NA> Overall AGE  continuous         N               N
      92                    <NA> Overall AGE  continuous      mean            Mean
      93                    <NA> Overall AGE  continuous        sd              SD
      94                    <NA> Overall AGE  continuous    median          Median
      95                    <NA> Overall AGE  continuous       p25 25th Percentile
      96                    <NA> Overall AGE  continuous       p75 75th Percentile
      97                    <NA> Overall AGE  continuous       min             Min
      98                    <NA> Overall AGE  continuous       max             Max
      99                    <NA> Overall AGE  continuous         N               N
      100                   <NA> Overall AGE  continuous      mean            Mean
      101                   <NA> Overall AGE  continuous        sd              SD
      102                   <NA> Overall AGE  continuous    median          Median
      103                   <NA> Overall AGE  continuous       p25 25th Percentile
      104                   <NA> Overall AGE  continuous       p75 75th Percentile
      105                   <NA> Overall AGE  continuous       min             Min
      106                   <NA> Overall AGE  continuous       max             Max
      107                   <NA> Overall AGE  continuous         N               N
      108                   <NA> Overall AGE  continuous      mean            Mean
      109                   <NA> Overall AGE  continuous        sd              SD
      110                   <NA> Overall AGE  continuous    median          Median
      111                   <NA> Overall AGE  continuous       p25 25th Percentile
      112                   <NA> Overall AGE  continuous       p75 75th Percentile
      113                   <NA> Overall AGE  continuous       min             Min
      114                   <NA> Overall AGE  continuous       max             Max
      115                   <NA> Overall AGE  continuous         N               N
      116                   <NA> Overall AGE  continuous      mean            Mean
      117                   <NA> Overall AGE  continuous        sd              SD
      118                   <NA> Overall AGE  continuous    median          Median
      119                   <NA> Overall AGE  continuous       p25 25th Percentile
      120                   <NA> Overall AGE  continuous       p75 75th Percentile
      121                   <NA> Overall AGE  continuous       min             Min
      122                   <NA> Overall AGE  continuous       max             Max
      123                   <NA> Overall AGE  continuous         N               N
      124                   <NA> Overall AGE  continuous      mean            Mean
      125                   <NA> Overall AGE  continuous        sd              SD
      126                   <NA> Overall AGE  continuous    median          Median
      127                   <NA> Overall AGE  continuous       p25 25th Percentile
      128                   <NA> Overall AGE  continuous       p75 75th Percentile
      129                   <NA> Overall AGE  continuous       min             Min
      130                   <NA> Overall AGE  continuous       max             Max
      131                   <NA> Overall AGE  continuous         N               N
      132                   <NA> Overall AGE  continuous      mean            Mean
      133                   <NA> Overall AGE  continuous        sd              SD
      134                   <NA> Overall AGE  continuous    median          Median
      135                   <NA> Overall AGE  continuous       p25 25th Percentile
      136                   <NA> Overall AGE  continuous       p75 75th Percentile
      137                   <NA> Overall AGE  continuous       min             Min
      138                   <NA> Overall AGE  continuous       max             Max
      139                   <NA>        <NA> categorical         n               n
      140                   <NA>        <NA> categorical         N               N
      141                   <NA>        <NA> categorical         p               %
      142                   <NA>        <NA> categorical         n               n
      143                   <NA>        <NA> categorical         N               N
      144                   <NA>        <NA> categorical         p               %
      145                   <NA>        <NA> categorical         n               n
      146                   <NA>        <NA> categorical         N               N
      147                   <NA>        <NA> categorical         p               %
      148                   <NA>        <NA> categorical         n               n
      149                   <NA>        <NA> categorical         N               N
      150                   <NA>        <NA> categorical         p               %
      151                   <NA>        <NA> categorical         n               n
      152                   <NA>        <NA> categorical         N               N
      153                   <NA>        <NA> categorical         p               %
                stat
      1            0
      2           53
      3            0
      4            5
      5           53
      6   0.09433962
      7           48
      8           53
      9    0.9056604
      10           0
      11          33
      12           0
      13           3
      14          33
      15  0.09090909
      16          30
      17          33
      18   0.9090909
      19           0
      20          40
      21           0
      22           6
      23          40
      24        0.15
      25          34
      26          40
      27        0.85
      28           1
      29          44
      30  0.02272727
      31           3
      32          44
      33  0.06818182
      34          40
      35          44
      36   0.9090909
      37           0
      38          50
      39           0
      40           6
      41          50
      42        0.12
      43          44
      44          50
      45        0.88
      46           0
      47          34
      48           0
      49           0
      50          34
      51           0
      52          34
      53          34
      54           1
      55           2
      56          53
      57  0.03773585
      58          51
      59          53
      60   0.9622642
      61           1
      62          33
      63  0.03030303
      64          32
      65          33
      66    0.969697
      67           1
      68          40
      69       0.025
      70          39
      71          40
      72       0.975
      73           2
      74          44
      75  0.04545455
      76          42
      77          44
      78   0.9545455
      79           4
      80          50
      81        0.08
      82          46
      83          50
      84        0.92
      85           2
      86          34
      87  0.05882353
      88          32
      89          34
      90   0.9411765
      91          53
      92    76.35849
      93    8.733433
      94          78
      95          70
      96          84
      97          59
      98          89
      99          33
      100   73.36364
      101   8.146388
      102         74
      103         69
      104         80
      105         52
      106         85
      107         40
      108     74.675
      109   7.667405
      110         76
      111         72
      112         79
      113         56
      114         88
      115         44
      116   74.11364
      117   8.158933
      118         77
      119         69
      120       80.5
      121         56
      122         86
      123         50
      124      75.68
      125   8.092425
      126       77.5
      127         72
      128         81
      129         54
      130         87
      131         34
      132   75.64706
      133   8.686047
      134       77.5
      135         68
      136         82
      137         51
      138         88
      139         86
      140        254
      141  0.3385827
      142         84
      143        254
      144  0.3307087
      145         84
      146        254
      147  0.3307087
      148        143
      149        254
      150  0.5629921
      151        111
      152        254
      153  0.4370079

---

    Code
      as.data.frame(rename_ard_columns(res_shuffle, "variable", col_lev = "label"))
    Output
                       TRT01A                             RACE                 ETHNIC
      1               Placebo AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      2               Placebo AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      3               Placebo AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      4               Placebo        BLACK OR AFRICAN AMERICAN                   <NA>
      5               Placebo        BLACK OR AFRICAN AMERICAN                   <NA>
      6               Placebo        BLACK OR AFRICAN AMERICAN                   <NA>
      7               Placebo                            WHITE                   <NA>
      8               Placebo                            WHITE                   <NA>
      9               Placebo                            WHITE                   <NA>
      10 Xanomeline High Dose AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      11 Xanomeline High Dose AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      12 Xanomeline High Dose AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      13 Xanomeline High Dose        BLACK OR AFRICAN AMERICAN                   <NA>
      14 Xanomeline High Dose        BLACK OR AFRICAN AMERICAN                   <NA>
      15 Xanomeline High Dose        BLACK OR AFRICAN AMERICAN                   <NA>
      16 Xanomeline High Dose                            WHITE                   <NA>
      17 Xanomeline High Dose                            WHITE                   <NA>
      18 Xanomeline High Dose                            WHITE                   <NA>
      19  Xanomeline Low Dose AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      20  Xanomeline Low Dose AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      21  Xanomeline Low Dose AMERICAN INDIAN OR ALASKA NATIVE                   <NA>
      22  Xanomeline Low Dose        BLACK OR AFRICAN AMERICAN                   <NA>
      23  Xanomeline Low Dose        BLACK OR AFRICAN AMERICAN                   <NA>
      24  Xanomeline Low Dose        BLACK OR AFRICAN AMERICAN                   <NA>
      25  Xanomeline Low Dose                            WHITE                   <NA>
      26  Xanomeline Low Dose                            WHITE                   <NA>
      27  Xanomeline Low Dose                            WHITE                   <NA>
      28              Placebo                             <NA>     HISPANIC OR LATINO
      29              Placebo                             <NA>     HISPANIC OR LATINO
      30              Placebo                             <NA>     HISPANIC OR LATINO
      31              Placebo                             <NA> NOT HISPANIC OR LATINO
      32              Placebo                             <NA> NOT HISPANIC OR LATINO
      33              Placebo                             <NA> NOT HISPANIC OR LATINO
      34 Xanomeline High Dose                             <NA>     HISPANIC OR LATINO
      35 Xanomeline High Dose                             <NA>     HISPANIC OR LATINO
      36 Xanomeline High Dose                             <NA>     HISPANIC OR LATINO
      37 Xanomeline High Dose                             <NA> NOT HISPANIC OR LATINO
      38 Xanomeline High Dose                             <NA> NOT HISPANIC OR LATINO
      39 Xanomeline High Dose                             <NA> NOT HISPANIC OR LATINO
      40  Xanomeline Low Dose                             <NA>     HISPANIC OR LATINO
      41  Xanomeline Low Dose                             <NA>     HISPANIC OR LATINO
      42  Xanomeline Low Dose                             <NA>     HISPANIC OR LATINO
      43  Xanomeline Low Dose                             <NA> NOT HISPANIC OR LATINO
      44  Xanomeline Low Dose                             <NA> NOT HISPANIC OR LATINO
      45  Xanomeline Low Dose                             <NA> NOT HISPANIC OR LATINO
      46              Placebo                             <NA>                   <NA>
      47              Placebo                             <NA>                   <NA>
      48              Placebo                             <NA>                   <NA>
      49              Placebo                             <NA>                   <NA>
      50              Placebo                             <NA>                   <NA>
      51              Placebo                             <NA>                   <NA>
      52              Placebo                             <NA>                   <NA>
      53              Placebo                             <NA>                   <NA>
      54 Xanomeline High Dose                             <NA>                   <NA>
      55 Xanomeline High Dose                             <NA>                   <NA>
      56 Xanomeline High Dose                             <NA>                   <NA>
      57 Xanomeline High Dose                             <NA>                   <NA>
      58 Xanomeline High Dose                             <NA>                   <NA>
      59 Xanomeline High Dose                             <NA>                   <NA>
      60 Xanomeline High Dose                             <NA>                   <NA>
      61 Xanomeline High Dose                             <NA>                   <NA>
      62  Xanomeline Low Dose                             <NA>                   <NA>
      63  Xanomeline Low Dose                             <NA>                   <NA>
      64  Xanomeline Low Dose                             <NA>                   <NA>
      65  Xanomeline Low Dose                             <NA>                   <NA>
      66  Xanomeline Low Dose                             <NA>                   <NA>
      67  Xanomeline Low Dose                             <NA>                   <NA>
      68  Xanomeline Low Dose                             <NA>                   <NA>
      69  Xanomeline Low Dose                             <NA>                   <NA>
      70              Placebo                             <NA>                   <NA>
      71              Placebo                             <NA>                   <NA>
      72              Placebo                             <NA>                   <NA>
      73 Xanomeline High Dose                             <NA>                   <NA>
      74 Xanomeline High Dose                             <NA>                   <NA>
      75 Xanomeline High Dose                             <NA>                   <NA>
      76  Xanomeline Low Dose                             <NA>                   <NA>
      77  Xanomeline Low Dose                             <NA>                   <NA>
      78  Xanomeline Low Dose                             <NA>                   <NA>
                     AGE     context stat_name         stat
      1             <NA> categorical         n   0.00000000
      2             <NA> categorical         N  86.00000000
      3             <NA> categorical         p   0.00000000
      4             <NA> categorical         n   8.00000000
      5             <NA> categorical         N  86.00000000
      6             <NA> categorical         p   0.09302326
      7             <NA> categorical         n  78.00000000
      8             <NA> categorical         N  86.00000000
      9             <NA> categorical         p   0.90697674
      10            <NA> categorical         n   1.00000000
      11            <NA> categorical         N  84.00000000
      12            <NA> categorical         p   0.01190476
      13            <NA> categorical         n   9.00000000
      14            <NA> categorical         N  84.00000000
      15            <NA> categorical         p   0.10714286
      16            <NA> categorical         n  74.00000000
      17            <NA> categorical         N  84.00000000
      18            <NA> categorical         p   0.88095238
      19            <NA> categorical         n   0.00000000
      20            <NA> categorical         N  84.00000000
      21            <NA> categorical         p   0.00000000
      22            <NA> categorical         n   6.00000000
      23            <NA> categorical         N  84.00000000
      24            <NA> categorical         p   0.07142857
      25            <NA> categorical         n  78.00000000
      26            <NA> categorical         N  84.00000000
      27            <NA> categorical         p   0.92857143
      28            <NA> categorical         n   3.00000000
      29            <NA> categorical         N  86.00000000
      30            <NA> categorical         p   0.03488372
      31            <NA> categorical         n  83.00000000
      32            <NA> categorical         N  86.00000000
      33            <NA> categorical         p   0.96511628
      34            <NA> categorical         n   3.00000000
      35            <NA> categorical         N  84.00000000
      36            <NA> categorical         p   0.03571429
      37            <NA> categorical         n  81.00000000
      38            <NA> categorical         N  84.00000000
      39            <NA> categorical         p   0.96428571
      40            <NA> categorical         n   6.00000000
      41            <NA> categorical         N  84.00000000
      42            <NA> categorical         p   0.07142857
      43            <NA> categorical         n  78.00000000
      44            <NA> categorical         N  84.00000000
      45            <NA> categorical         p   0.92857143
      46               N  continuous         N  86.00000000
      47            Mean  continuous      mean  75.20930233
      48              SD  continuous        sd   8.59016713
      49          Median  continuous    median  76.00000000
      50 25th Percentile  continuous       p25  69.00000000
      51 75th Percentile  continuous       p75  82.00000000
      52             Min  continuous       min  52.00000000
      53             Max  continuous       max  89.00000000
      54               N  continuous         N  84.00000000
      55            Mean  continuous      mean  74.38095238
      56              SD  continuous        sd   7.88609385
      57          Median  continuous    median  76.00000000
      58 25th Percentile  continuous       p25  70.50000000
      59 75th Percentile  continuous       p75  80.00000000
      60             Min  continuous       min  56.00000000
      61             Max  continuous       max  88.00000000
      62               N  continuous         N  84.00000000
      63            Mean  continuous      mean  75.66666667
      64              SD  continuous        sd   8.28605060
      65          Median  continuous    median  77.50000000
      66 25th Percentile  continuous       p25  71.00000000
      67 75th Percentile  continuous       p75  82.00000000
      68             Min  continuous       min  51.00000000
      69             Max  continuous       max  88.00000000
      70            <NA> categorical         n  86.00000000
      71            <NA> categorical         N 254.00000000
      72            <NA> categorical         p   0.33858268
      73            <NA> categorical         n  84.00000000
      74            <NA> categorical         N 254.00000000
      75            <NA> categorical         p   0.33070866
      76            <NA> categorical         n  84.00000000
      77            <NA> categorical         N 254.00000000
      78            <NA> categorical         p   0.33070866

