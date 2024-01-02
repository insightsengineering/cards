# shuffle/trim works

    Code
      ard_simple_shuffled
    Output
        variable    context stat_name      stat_label statistic statistic_fmt_fn
      1      AGE continuous         N               N       254                0
      2      AGE continuous      mean            Mean  75.08661                1
      3      AGE continuous        sd              SD  8.246234                1
      4      AGE continuous    median          Median        77                1
      5      AGE continuous       p25 25th Percentile        70                1
      6      AGE continuous       p75 75th Percentile        81                1
      7      AGE continuous       min             Min        51                1
      8      AGE continuous       max             Max        89                1
        warning error
      1    NULL  NULL
      2    NULL  NULL
      3    NULL  NULL
      4    NULL  NULL
      5    NULL  NULL
      6    NULL  NULL
      7    NULL  NULL
      8    NULL  NULL

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
      41              Placebo   AGEGR1                65-80 categorical         p
      42              Placebo   AGEGR1                  <65 categorical         n
      43              Placebo   AGEGR1                  <65 categorical         p
      44              Placebo   AGEGR1                  >80 categorical         n
      45              Placebo   AGEGR1                  >80 categorical         p
      46              Placebo   AGEGR1                    N categorical         N
      47              Placebo   AGEGR1        Vector Length     missing     N_obs
      48              Placebo   AGEGR1            N Missing     missing    N_miss
      49              Placebo   AGEGR1        N Non-missing     missing N_nonmiss
      50              Placebo   AGEGR1            % Missing     missing    p_miss
      51              Placebo   AGEGR1        % Non-missing     missing p_nonmiss
      52 Xanomeline High Dose   AGEGR1                65-80 categorical         n
      53 Xanomeline High Dose   AGEGR1                65-80 categorical         p
      54 Xanomeline High Dose   AGEGR1                  <65 categorical         n
      55 Xanomeline High Dose   AGEGR1                  <65 categorical         p
      56 Xanomeline High Dose   AGEGR1                  >80 categorical         n
      57 Xanomeline High Dose   AGEGR1                  >80 categorical         p
      58 Xanomeline High Dose   AGEGR1                    N categorical         N
      59 Xanomeline High Dose   AGEGR1        Vector Length     missing     N_obs
      60 Xanomeline High Dose   AGEGR1            N Missing     missing    N_miss
      61 Xanomeline High Dose   AGEGR1        N Non-missing     missing N_nonmiss
      62 Xanomeline High Dose   AGEGR1            % Missing     missing    p_miss
      63 Xanomeline High Dose   AGEGR1        % Non-missing     missing p_nonmiss
      64  Xanomeline Low Dose   AGEGR1                65-80 categorical         n
      65  Xanomeline Low Dose   AGEGR1                65-80 categorical         p
      66  Xanomeline Low Dose   AGEGR1                  <65 categorical         n
      67  Xanomeline Low Dose   AGEGR1                  <65 categorical         p
      68  Xanomeline Low Dose   AGEGR1                  >80 categorical         n
      69  Xanomeline Low Dose   AGEGR1                  >80 categorical         p
      70  Xanomeline Low Dose   AGEGR1                    N categorical         N
      71  Xanomeline Low Dose   AGEGR1        Vector Length     missing     N_obs
      72  Xanomeline Low Dose   AGEGR1            N Missing     missing    N_miss
      73  Xanomeline Low Dose   AGEGR1        N Non-missing     missing N_nonmiss
      74  Xanomeline Low Dose   AGEGR1            % Missing     missing    p_miss
      75  Xanomeline Low Dose   AGEGR1        % Non-missing     missing p_nonmiss
      76              Placebo      ARM              Placebo categorical         n
      77              Placebo      ARM              Placebo categorical         p
      78 Xanomeline High Dose      ARM Xanomeline High Dose categorical         n
      79 Xanomeline High Dose      ARM Xanomeline High Dose categorical         p
      80  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         n
      81  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         p
      82                 <NA>      ARM                    N categorical         N
           statistic
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
      41   0.4883721
      42  14.0000000
      43   0.1627907
      44  30.0000000
      45   0.3488372
      46  86.0000000
      47  86.0000000
      48   0.0000000
      49  86.0000000
      50   0.0000000
      51   1.0000000
      52  55.0000000
      53   0.6547619
      54  11.0000000
      55   0.1309524
      56  18.0000000
      57   0.2142857
      58  84.0000000
      59  84.0000000
      60   0.0000000
      61  84.0000000
      62   0.0000000
      63   1.0000000
      64  47.0000000
      65   0.5595238
      66   8.0000000
      67   0.0952381
      68  29.0000000
      69   0.3452381
      70  84.0000000
      71  84.0000000
      72   0.0000000
      73  84.0000000
      74   0.0000000
      75   1.0000000
      76  86.0000000
      77   0.3385827
      78  84.0000000
      79   0.3307087
      80  84.0000000
      81   0.3307087
      82 254.0000000

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
      41              Placebo   AGEGR1                65-80 categorical         p
      42              Placebo   AGEGR1                  <65 categorical         n
      43              Placebo   AGEGR1                  <65 categorical         p
      44              Placebo   AGEGR1                  >80 categorical         n
      45              Placebo   AGEGR1                  >80 categorical         p
      46              Placebo   AGEGR1                    N categorical         N
      47              Placebo   AGEGR1        Vector Length     missing     N_obs
      48              Placebo   AGEGR1            N Missing     missing    N_miss
      49              Placebo   AGEGR1        N Non-missing     missing N_nonmiss
      50              Placebo   AGEGR1            % Missing     missing    p_miss
      51              Placebo   AGEGR1        % Non-missing     missing p_nonmiss
      52 Xanomeline High Dose   AGEGR1                65-80 categorical         n
      53 Xanomeline High Dose   AGEGR1                65-80 categorical         p
      54 Xanomeline High Dose   AGEGR1                  <65 categorical         n
      55 Xanomeline High Dose   AGEGR1                  <65 categorical         p
      56 Xanomeline High Dose   AGEGR1                  >80 categorical         n
      57 Xanomeline High Dose   AGEGR1                  >80 categorical         p
      58 Xanomeline High Dose   AGEGR1                    N categorical         N
      59 Xanomeline High Dose   AGEGR1        Vector Length     missing     N_obs
      60 Xanomeline High Dose   AGEGR1            N Missing     missing    N_miss
      61 Xanomeline High Dose   AGEGR1        N Non-missing     missing N_nonmiss
      62 Xanomeline High Dose   AGEGR1            % Missing     missing    p_miss
      63 Xanomeline High Dose   AGEGR1        % Non-missing     missing p_nonmiss
      64  Xanomeline Low Dose   AGEGR1                65-80 categorical         n
      65  Xanomeline Low Dose   AGEGR1                65-80 categorical         p
      66  Xanomeline Low Dose   AGEGR1                  <65 categorical         n
      67  Xanomeline Low Dose   AGEGR1                  <65 categorical         p
      68  Xanomeline Low Dose   AGEGR1                  >80 categorical         n
      69  Xanomeline Low Dose   AGEGR1                  >80 categorical         p
      70  Xanomeline Low Dose   AGEGR1                    N categorical         N
      71  Xanomeline Low Dose   AGEGR1        Vector Length     missing     N_obs
      72  Xanomeline Low Dose   AGEGR1            N Missing     missing    N_miss
      73  Xanomeline Low Dose   AGEGR1        N Non-missing     missing N_nonmiss
      74  Xanomeline Low Dose   AGEGR1            % Missing     missing    p_miss
      75  Xanomeline Low Dose   AGEGR1        % Non-missing     missing p_nonmiss
      76              Placebo      ARM              Placebo categorical         n
      77              Placebo      ARM              Placebo categorical         p
      78 Xanomeline High Dose      ARM Xanomeline High Dose categorical         n
      79 Xanomeline High Dose      ARM Xanomeline High Dose categorical         p
      80  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         n
      81  Xanomeline Low Dose      ARM  Xanomeline Low Dose categorical         p
      82                 <NA>      ARM                    N categorical         N
           statistic
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
      41   0.4883721
      42  14.0000000
      43   0.1627907
      44  30.0000000
      45   0.3488372
      46  86.0000000
      47  86.0000000
      48   0.0000000
      49  86.0000000
      50   0.0000000
      51   1.0000000
      52  55.0000000
      53   0.6547619
      54  11.0000000
      55   0.1309524
      56  18.0000000
      57   0.2142857
      58  84.0000000
      59  84.0000000
      60   0.0000000
      61  84.0000000
      62   0.0000000
      63   1.0000000
      64  47.0000000
      65   0.5595238
      66   8.0000000
      67   0.0952381
      68  29.0000000
      69   0.3452381
      70  84.0000000
      71  84.0000000
      72   0.0000000
      73  84.0000000
      74   0.0000000
      75   1.0000000
      76  86.0000000
      77   0.3385827
      78  84.0000000
      79   0.3307087
      80  84.0000000
      81   0.3307087
      82 254.0000000

# shuffle_ard notifies user about warnings/errors before dropping

    Code
      as.data.frame(shuffle_ard(ard_ttest(data = ADSL, by = "ARM", variable = "AGEGR1")))
    Message
      "error" column contains messages that will be removed.
    Output
        variable context  stat_name statistic
      1   AGEGR1   ttest         mu      0.00
      2   AGEGR1   ttest conf.level      0.95

