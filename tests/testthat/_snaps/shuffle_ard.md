# shuffle/trim works

    Code
      ard_simple_shuffled
    Output
        variable    context stat_name stat_label     stat fmt_fn warning error
      1      AGE continuous         N          N      254      0    NULL  NULL
      2      AGE continuous      mean       Mean 75.08661      1    NULL  NULL
      3      AGE continuous        sd         SD 8.246234      1    NULL  NULL
      4      AGE continuous    median     Median       77      1    NULL  NULL
      5      AGE continuous       p25         Q1       70      1    NULL  NULL
      6      AGE continuous       p75         Q3       81      1    NULL  NULL
      7      AGE continuous       min        Min       51      1    NULL  NULL
      8      AGE continuous       max        Max       89      1    NULL  NULL

---

    Code
      ard_shuffled
    Output
                          ARM variable                label     context stat_name
      1               Placebo      AGE                    N  continuous         N
      2               Placebo      AGE             Mean(SD)  continuous      mean
      3               Placebo      AGE             Mean(SD)  continuous        sd
      4               Placebo      AGE               median  continuous    median
      5               Placebo      AGE                  p25  continuous       p25
      6               Placebo      AGE                  p75  continuous       p75
      7               Placebo      AGE                  min  continuous       min
      8               Placebo      AGE                  max  continuous       max
      9               Placebo      AGE        Vector Length     missing     N_obs
      10              Placebo      AGE            N Missing     missing    N_miss
      11              Placebo      AGE        N Non-missing     missing N_nonmiss
      12              Placebo      AGE            % Missing     missing    p_miss
      13              Placebo      AGE        % Non-missing     missing p_nonmiss
      14 Xanomeline High Dose      AGE                    N  continuous         N
      15 Xanomeline High Dose      AGE             Mean(SD)  continuous      mean
      16 Xanomeline High Dose      AGE             Mean(SD)  continuous        sd
      17 Xanomeline High Dose      AGE               median  continuous    median
      18 Xanomeline High Dose      AGE                  p25  continuous       p25
      19 Xanomeline High Dose      AGE                  p75  continuous       p75
      20 Xanomeline High Dose      AGE                  min  continuous       min
      21 Xanomeline High Dose      AGE                  max  continuous       max
      22 Xanomeline High Dose      AGE        Vector Length     missing     N_obs
      23 Xanomeline High Dose      AGE            N Missing     missing    N_miss
      24 Xanomeline High Dose      AGE        N Non-missing     missing N_nonmiss
      25 Xanomeline High Dose      AGE            % Missing     missing    p_miss
      26 Xanomeline High Dose      AGE        % Non-missing     missing p_nonmiss
      27  Xanomeline Low Dose      AGE                    N  continuous         N
      28  Xanomeline Low Dose      AGE             Mean(SD)  continuous      mean
      29  Xanomeline Low Dose      AGE             Mean(SD)  continuous        sd
      30  Xanomeline Low Dose      AGE               median  continuous    median
      31  Xanomeline Low Dose      AGE                  p25  continuous       p25
      32  Xanomeline Low Dose      AGE                  p75  continuous       p75
      33  Xanomeline Low Dose      AGE                  min  continuous       min
      34  Xanomeline Low Dose      AGE                  max  continuous       max
      35  Xanomeline Low Dose      AGE        Vector Length     missing     N_obs
      36  Xanomeline Low Dose      AGE            N Missing     missing    N_miss
      37  Xanomeline Low Dose      AGE        N Non-missing     missing N_nonmiss
      38  Xanomeline Low Dose      AGE            % Missing     missing    p_miss
      39  Xanomeline Low Dose      AGE        % Non-missing     missing p_nonmiss
      40              Placebo   AGEGR1                65-80 categorical         n
      41              Placebo   AGEGR1                65-80 categorical         N
      42              Placebo   AGEGR1                65-80 categorical         p
      43              Placebo   AGEGR1                  <65 categorical         n
      44              Placebo   AGEGR1                  <65 categorical         N
      45              Placebo   AGEGR1                  <65 categorical         p
      46              Placebo   AGEGR1                  >80 categorical         n
      47              Placebo   AGEGR1                  >80 categorical         N
      48              Placebo   AGEGR1                  >80 categorical         p
      49              Placebo   AGEGR1        Vector Length     missing     N_obs
      50              Placebo   AGEGR1            N Missing     missing    N_miss
      51              Placebo   AGEGR1        N Non-missing     missing N_nonmiss
      52              Placebo   AGEGR1            % Missing     missing    p_miss
      53              Placebo   AGEGR1        % Non-missing     missing p_nonmiss
      54 Xanomeline High Dose   AGEGR1                65-80 categorical         n
      55 Xanomeline High Dose   AGEGR1                65-80 categorical         N
      56 Xanomeline High Dose   AGEGR1                65-80 categorical         p
      57 Xanomeline High Dose   AGEGR1                  <65 categorical         n
      58 Xanomeline High Dose   AGEGR1                  <65 categorical         N
      59 Xanomeline High Dose   AGEGR1                  <65 categorical         p
      60 Xanomeline High Dose   AGEGR1                  >80 categorical         n
      61 Xanomeline High Dose   AGEGR1                  >80 categorical         N
      62 Xanomeline High Dose   AGEGR1                  >80 categorical         p
      63 Xanomeline High Dose   AGEGR1        Vector Length     missing     N_obs
      64 Xanomeline High Dose   AGEGR1            N Missing     missing    N_miss
      65 Xanomeline High Dose   AGEGR1        N Non-missing     missing N_nonmiss
      66 Xanomeline High Dose   AGEGR1            % Missing     missing    p_miss
      67 Xanomeline High Dose   AGEGR1        % Non-missing     missing p_nonmiss
      68  Xanomeline Low Dose   AGEGR1                65-80 categorical         n
      69  Xanomeline Low Dose   AGEGR1                65-80 categorical         N
      70  Xanomeline Low Dose   AGEGR1                65-80 categorical         p
      71  Xanomeline Low Dose   AGEGR1                  <65 categorical         n
      72  Xanomeline Low Dose   AGEGR1                  <65 categorical         N
      73  Xanomeline Low Dose   AGEGR1                  <65 categorical         p
      74  Xanomeline Low Dose   AGEGR1                  >80 categorical         n
      75  Xanomeline Low Dose   AGEGR1                  >80 categorical         N
      76  Xanomeline Low Dose   AGEGR1                  >80 categorical         p
      77  Xanomeline Low Dose   AGEGR1        Vector Length     missing     N_obs
      78  Xanomeline Low Dose   AGEGR1            N Missing     missing    N_miss
      79  Xanomeline Low Dose   AGEGR1        N Non-missing     missing N_nonmiss
      80  Xanomeline Low Dose   AGEGR1            % Missing     missing    p_miss
      81  Xanomeline Low Dose   AGEGR1        % Non-missing     missing p_nonmiss
      82              Placebo      ARM              Placebo categorical         n
      83              Placebo      ARM              Placebo categorical         N
      84              Placebo      ARM              Placebo categorical         p
      85 Xanomeline High Dose      ARM Xanomeline High Dose categorical         n
      86 Xanomeline High Dose      ARM Xanomeline High Dose categorical         N
      87 Xanomeline High Dose      ARM Xanomeline High Dose categorical         p
      88  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         n
      89  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         N
      90  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         p
                stat
      1   86.0000000
      2   75.2093023
      3    8.5901671
      4   76.0000000
      5   69.0000000
      6   82.0000000
      7   52.0000000
      8   89.0000000
      9   86.0000000
      10   0.0000000
      11  86.0000000
      12   0.0000000
      13   1.0000000
      14  84.0000000
      15  74.3809524
      16   7.8860938
      17  76.0000000
      18  70.5000000
      19  80.0000000
      20  56.0000000
      21  88.0000000
      22  84.0000000
      23   0.0000000
      24  84.0000000
      25   0.0000000
      26   1.0000000
      27  84.0000000
      28  75.6666667
      29   8.2860506
      30  77.5000000
      31  71.0000000
      32  82.0000000
      33  51.0000000
      34  88.0000000
      35  84.0000000
      36   0.0000000
      37  84.0000000
      38   0.0000000
      39   1.0000000
      40  42.0000000
      41  86.0000000
      42   0.4883721
      43  14.0000000
      44  86.0000000
      45   0.1627907
      46  30.0000000
      47  86.0000000
      48   0.3488372
      49  86.0000000
      50   0.0000000
      51  86.0000000
      52   0.0000000
      53   1.0000000
      54  55.0000000
      55  84.0000000
      56   0.6547619
      57  11.0000000
      58  84.0000000
      59   0.1309524
      60  18.0000000
      61  84.0000000
      62   0.2142857
      63  84.0000000
      64   0.0000000
      65  84.0000000
      66   0.0000000
      67   1.0000000
      68  47.0000000
      69  84.0000000
      70   0.5595238
      71   8.0000000
      72  84.0000000
      73   0.0952381
      74  29.0000000
      75  84.0000000
      76   0.3452381
      77  84.0000000
      78   0.0000000
      79  84.0000000
      80   0.0000000
      81   1.0000000
      82  86.0000000
      83 254.0000000
      84   0.3385827
      85  84.0000000
      86 254.0000000
      87   0.3307087
      88  84.0000000
      89 254.0000000
      90   0.3307087

---

    Code
      ard_shuff_trim
    Output
                          ARM variable                label     context stat_name
      1               Placebo      AGE                    N  continuous         N
      2               Placebo      AGE             Mean(SD)  continuous      mean
      3               Placebo      AGE             Mean(SD)  continuous        sd
      4               Placebo      AGE               median  continuous    median
      5               Placebo      AGE                  p25  continuous       p25
      6               Placebo      AGE                  p75  continuous       p75
      7               Placebo      AGE                  min  continuous       min
      8               Placebo      AGE                  max  continuous       max
      9               Placebo      AGE        Vector Length     missing     N_obs
      10              Placebo      AGE            N Missing     missing    N_miss
      11              Placebo      AGE        N Non-missing     missing N_nonmiss
      12              Placebo      AGE            % Missing     missing    p_miss
      13              Placebo      AGE        % Non-missing     missing p_nonmiss
      14 Xanomeline High Dose      AGE                    N  continuous         N
      15 Xanomeline High Dose      AGE             Mean(SD)  continuous      mean
      16 Xanomeline High Dose      AGE             Mean(SD)  continuous        sd
      17 Xanomeline High Dose      AGE               median  continuous    median
      18 Xanomeline High Dose      AGE                  p25  continuous       p25
      19 Xanomeline High Dose      AGE                  p75  continuous       p75
      20 Xanomeline High Dose      AGE                  min  continuous       min
      21 Xanomeline High Dose      AGE                  max  continuous       max
      22 Xanomeline High Dose      AGE        Vector Length     missing     N_obs
      23 Xanomeline High Dose      AGE            N Missing     missing    N_miss
      24 Xanomeline High Dose      AGE        N Non-missing     missing N_nonmiss
      25 Xanomeline High Dose      AGE            % Missing     missing    p_miss
      26 Xanomeline High Dose      AGE        % Non-missing     missing p_nonmiss
      27  Xanomeline Low Dose      AGE                    N  continuous         N
      28  Xanomeline Low Dose      AGE             Mean(SD)  continuous      mean
      29  Xanomeline Low Dose      AGE             Mean(SD)  continuous        sd
      30  Xanomeline Low Dose      AGE               median  continuous    median
      31  Xanomeline Low Dose      AGE                  p25  continuous       p25
      32  Xanomeline Low Dose      AGE                  p75  continuous       p75
      33  Xanomeline Low Dose      AGE                  min  continuous       min
      34  Xanomeline Low Dose      AGE                  max  continuous       max
      35  Xanomeline Low Dose      AGE        Vector Length     missing     N_obs
      36  Xanomeline Low Dose      AGE            N Missing     missing    N_miss
      37  Xanomeline Low Dose      AGE        N Non-missing     missing N_nonmiss
      38  Xanomeline Low Dose      AGE            % Missing     missing    p_miss
      39  Xanomeline Low Dose      AGE        % Non-missing     missing p_nonmiss
      40              Placebo   AGEGR1                65-80 categorical         n
      41              Placebo   AGEGR1                65-80 categorical         N
      42              Placebo   AGEGR1                65-80 categorical         p
      43              Placebo   AGEGR1                  <65 categorical         n
      44              Placebo   AGEGR1                  <65 categorical         N
      45              Placebo   AGEGR1                  <65 categorical         p
      46              Placebo   AGEGR1                  >80 categorical         n
      47              Placebo   AGEGR1                  >80 categorical         N
      48              Placebo   AGEGR1                  >80 categorical         p
      49              Placebo   AGEGR1        Vector Length     missing     N_obs
      50              Placebo   AGEGR1            N Missing     missing    N_miss
      51              Placebo   AGEGR1        N Non-missing     missing N_nonmiss
      52              Placebo   AGEGR1            % Missing     missing    p_miss
      53              Placebo   AGEGR1        % Non-missing     missing p_nonmiss
      54 Xanomeline High Dose   AGEGR1                65-80 categorical         n
      55 Xanomeline High Dose   AGEGR1                65-80 categorical         N
      56 Xanomeline High Dose   AGEGR1                65-80 categorical         p
      57 Xanomeline High Dose   AGEGR1                  <65 categorical         n
      58 Xanomeline High Dose   AGEGR1                  <65 categorical         N
      59 Xanomeline High Dose   AGEGR1                  <65 categorical         p
      60 Xanomeline High Dose   AGEGR1                  >80 categorical         n
      61 Xanomeline High Dose   AGEGR1                  >80 categorical         N
      62 Xanomeline High Dose   AGEGR1                  >80 categorical         p
      63 Xanomeline High Dose   AGEGR1        Vector Length     missing     N_obs
      64 Xanomeline High Dose   AGEGR1            N Missing     missing    N_miss
      65 Xanomeline High Dose   AGEGR1        N Non-missing     missing N_nonmiss
      66 Xanomeline High Dose   AGEGR1            % Missing     missing    p_miss
      67 Xanomeline High Dose   AGEGR1        % Non-missing     missing p_nonmiss
      68  Xanomeline Low Dose   AGEGR1                65-80 categorical         n
      69  Xanomeline Low Dose   AGEGR1                65-80 categorical         N
      70  Xanomeline Low Dose   AGEGR1                65-80 categorical         p
      71  Xanomeline Low Dose   AGEGR1                  <65 categorical         n
      72  Xanomeline Low Dose   AGEGR1                  <65 categorical         N
      73  Xanomeline Low Dose   AGEGR1                  <65 categorical         p
      74  Xanomeline Low Dose   AGEGR1                  >80 categorical         n
      75  Xanomeline Low Dose   AGEGR1                  >80 categorical         N
      76  Xanomeline Low Dose   AGEGR1                  >80 categorical         p
      77  Xanomeline Low Dose   AGEGR1        Vector Length     missing     N_obs
      78  Xanomeline Low Dose   AGEGR1            N Missing     missing    N_miss
      79  Xanomeline Low Dose   AGEGR1        N Non-missing     missing N_nonmiss
      80  Xanomeline Low Dose   AGEGR1            % Missing     missing    p_miss
      81  Xanomeline Low Dose   AGEGR1        % Non-missing     missing p_nonmiss
      82              Placebo      ARM              Placebo categorical         n
      83              Placebo      ARM              Placebo categorical         N
      84              Placebo      ARM              Placebo categorical         p
      85 Xanomeline High Dose      ARM Xanomeline High Dose categorical         n
      86 Xanomeline High Dose      ARM Xanomeline High Dose categorical         N
      87 Xanomeline High Dose      ARM Xanomeline High Dose categorical         p
      88  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         n
      89  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         N
      90  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         p
                stat
      1   86.0000000
      2   75.2093023
      3    8.5901671
      4   76.0000000
      5   69.0000000
      6   82.0000000
      7   52.0000000
      8   89.0000000
      9   86.0000000
      10   0.0000000
      11  86.0000000
      12   0.0000000
      13   1.0000000
      14  84.0000000
      15  74.3809524
      16   7.8860938
      17  76.0000000
      18  70.5000000
      19  80.0000000
      20  56.0000000
      21  88.0000000
      22  84.0000000
      23   0.0000000
      24  84.0000000
      25   0.0000000
      26   1.0000000
      27  84.0000000
      28  75.6666667
      29   8.2860506
      30  77.5000000
      31  71.0000000
      32  82.0000000
      33  51.0000000
      34  88.0000000
      35  84.0000000
      36   0.0000000
      37  84.0000000
      38   0.0000000
      39   1.0000000
      40  42.0000000
      41  86.0000000
      42   0.4883721
      43  14.0000000
      44  86.0000000
      45   0.1627907
      46  30.0000000
      47  86.0000000
      48   0.3488372
      49  86.0000000
      50   0.0000000
      51  86.0000000
      52   0.0000000
      53   1.0000000
      54  55.0000000
      55  84.0000000
      56   0.6547619
      57  11.0000000
      58  84.0000000
      59   0.1309524
      60  18.0000000
      61  84.0000000
      62   0.2142857
      63  84.0000000
      64   0.0000000
      65  84.0000000
      66   0.0000000
      67   1.0000000
      68  47.0000000
      69  84.0000000
      70   0.5595238
      71   8.0000000
      72  84.0000000
      73   0.0952381
      74  29.0000000
      75  84.0000000
      76   0.3452381
      77  84.0000000
      78   0.0000000
      79  84.0000000
      80   0.0000000
      81   1.0000000
      82  86.0000000
      83 254.0000000
      84   0.3385827
      85  84.0000000
      86 254.0000000
      87   0.3307087
      88  84.0000000
      89 254.0000000
      90   0.3307087

# shuffle_ard notifies user about warnings/errors before dropping

    Code
      shuffle_ard(ard_continuous(ADSL, variables = AGEGR1))
    Message
      "warning" column contains messages that will be removed.
    Output
      # A tibble: 4 x 4
        variable context    stat_name  stat
        <chr>    <chr>      <chr>     <dbl>
      1 AGEGR1   continuous N           254
      2 AGEGR1   continuous mean         NA
      3 AGEGR1   continuous sd           NA
      4 AGEGR1   continuous median       NA

# shuffle_ard fills missing group levels if the group is meaningful

    Code
      shuffle_ard(bind_ard(ard_continuous(ADSL, by = "ARM", variables = "AGE",
        statistic = ~ continuous_summary_fns("mean")), dplyr::tibble(group1 = "ARM",
        variable = "AGE", stat_name = "p", stat_label = "p", stat = list(0.05))))
    Output
      # A tibble: 4 x 5
        ARM                  variable context    stat_name  stat
        <chr>                <chr>    <chr>      <chr>     <dbl>
      1 Placebo              AGE      continuous mean      75.2 
      2 Xanomeline High Dose AGE      continuous mean      74.4 
      3 Xanomeline Low Dose  AGE      continuous mean      75.7 
      4 Overall ARM          AGE      <NA>       p          0.05

---

    Code
      shuffle_ard(bind_ard(ard_continuous(ADSL, variables = "AGE", statistic = ~
        continuous_summary_fns("mean")), dplyr::tibble(group1 = "ARM", variable = "AGE",
        stat_name = "p", stat_label = "p", stat = list(0.05))))
    Output
      # A tibble: 2 x 5
        ARM         variable context    stat_name  stat
        <chr>       <chr>    <chr>      <chr>     <dbl>
      1 Overall ARM AGE      <NA>       p          0.05
      2 <NA>        AGE      continuous mean      75.1 

---

    Code
      as.data.frame(shuffle_ard(bind_ard(ard_categorical(ADSL, by = ARM, variables = AGEGR1),
      ard_categorical(ADSL, variables = AGEGR1), ard_continuous(ADSL, by = SEX, variables = AGE),
      ard_continuous(ADSL, variables = AGE))))
    Output
                          ARM         SEX variable  label     context stat_name        stat
      1               Placebo        <NA>   AGEGR1  65-80 categorical         n  42.0000000
      2               Placebo        <NA>   AGEGR1  65-80 categorical         N  86.0000000
      3               Placebo        <NA>   AGEGR1  65-80 categorical         p   0.4883721
      4               Placebo        <NA>   AGEGR1    <65 categorical         n  14.0000000
      5               Placebo        <NA>   AGEGR1    <65 categorical         N  86.0000000
      6               Placebo        <NA>   AGEGR1    <65 categorical         p   0.1627907
      7               Placebo        <NA>   AGEGR1    >80 categorical         n  30.0000000
      8               Placebo        <NA>   AGEGR1    >80 categorical         N  86.0000000
      9               Placebo        <NA>   AGEGR1    >80 categorical         p   0.3488372
      10 Xanomeline High Dose        <NA>   AGEGR1  65-80 categorical         n  55.0000000
      11 Xanomeline High Dose        <NA>   AGEGR1  65-80 categorical         N  84.0000000
      12 Xanomeline High Dose        <NA>   AGEGR1  65-80 categorical         p   0.6547619
      13 Xanomeline High Dose        <NA>   AGEGR1    <65 categorical         n  11.0000000
      14 Xanomeline High Dose        <NA>   AGEGR1    <65 categorical         N  84.0000000
      15 Xanomeline High Dose        <NA>   AGEGR1    <65 categorical         p   0.1309524
      16 Xanomeline High Dose        <NA>   AGEGR1    >80 categorical         n  18.0000000
      17 Xanomeline High Dose        <NA>   AGEGR1    >80 categorical         N  84.0000000
      18 Xanomeline High Dose        <NA>   AGEGR1    >80 categorical         p   0.2142857
      19  Xanomeline Low Dose        <NA>   AGEGR1  65-80 categorical         n  47.0000000
      20  Xanomeline Low Dose        <NA>   AGEGR1  65-80 categorical         N  84.0000000
      21  Xanomeline Low Dose        <NA>   AGEGR1  65-80 categorical         p   0.5595238
      22  Xanomeline Low Dose        <NA>   AGEGR1    <65 categorical         n   8.0000000
      23  Xanomeline Low Dose        <NA>   AGEGR1    <65 categorical         N  84.0000000
      24  Xanomeline Low Dose        <NA>   AGEGR1    <65 categorical         p   0.0952381
      25  Xanomeline Low Dose        <NA>   AGEGR1    >80 categorical         n  29.0000000
      26  Xanomeline Low Dose        <NA>   AGEGR1    >80 categorical         N  84.0000000
      27  Xanomeline Low Dose        <NA>   AGEGR1    >80 categorical         p   0.3452381
      28          Overall ARM        <NA>   AGEGR1  65-80 categorical         n 144.0000000
      29          Overall ARM        <NA>   AGEGR1  65-80 categorical         N 254.0000000
      30          Overall ARM        <NA>   AGEGR1  65-80 categorical         p   0.5669291
      31          Overall ARM        <NA>   AGEGR1    <65 categorical         n  33.0000000
      32          Overall ARM        <NA>   AGEGR1    <65 categorical         N 254.0000000
      33          Overall ARM        <NA>   AGEGR1    <65 categorical         p   0.1299213
      34          Overall ARM        <NA>   AGEGR1    >80 categorical         n  77.0000000
      35          Overall ARM        <NA>   AGEGR1    >80 categorical         N 254.0000000
      36          Overall ARM        <NA>   AGEGR1    >80 categorical         p   0.3031496
      37                 <NA>           F      AGE      N  continuous         N 143.0000000
      38                 <NA>           F      AGE   Mean  continuous      mean  75.6503497
      39                 <NA>           F      AGE     SD  continuous        sd   8.1933146
      40                 <NA>           F      AGE Median  continuous    median  77.0000000
      41                 <NA>           F      AGE     Q1  continuous       p25  72.0000000
      42                 <NA>           F      AGE     Q3  continuous       p75  81.0000000
      43                 <NA>           F      AGE    Min  continuous       min  54.0000000
      44                 <NA>           F      AGE    Max  continuous       max  89.0000000
      45                 <NA>           M      AGE      N  continuous         N 111.0000000
      46                 <NA>           M      AGE   Mean  continuous      mean  74.3603604
      47                 <NA>           M      AGE     SD  continuous        sd   8.2943494
      48                 <NA>           M      AGE Median  continuous    median  77.0000000
      49                 <NA>           M      AGE     Q1  continuous       p25  69.0000000
      50                 <NA>           M      AGE     Q3  continuous       p75  81.0000000
      51                 <NA>           M      AGE    Min  continuous       min  51.0000000
      52                 <NA>           M      AGE    Max  continuous       max  88.0000000
      53                 <NA> Overall SEX      AGE      N  continuous         N 254.0000000
      54                 <NA> Overall SEX      AGE   Mean  continuous      mean  75.0866142
      55                 <NA> Overall SEX      AGE     SD  continuous        sd   8.2462339
      56                 <NA> Overall SEX      AGE Median  continuous    median  77.0000000
      57                 <NA> Overall SEX      AGE     Q1  continuous       p25  70.0000000
      58                 <NA> Overall SEX      AGE     Q3  continuous       p75  81.0000000
      59                 <NA> Overall SEX      AGE    Min  continuous       min  51.0000000
      60                 <NA> Overall SEX      AGE    Max  continuous       max  89.0000000

# shuffle_ard fills missing group levels if the group is meaningful for cardx output

    Code
      as.data.frame(shuffle_ard(ard_cardx))
    Output
                ARM         SEX variable          context stat_name       stat
      1 Overall ARM        <NA>   AGEGR1 stats_chisq_test statistic 5.07944167
      2 Overall ARM        <NA>   AGEGR1 stats_chisq_test   p.value 0.07888842
      3        <NA> Overall SEX   AGEGR1 stats_chisq_test statistic 1.03944200
      4        <NA> Overall SEX   AGEGR1 stats_chisq_test   p.value 0.59468644

