# ARD helpers messaging

    Code
      bind_ard(ard, ard, .update = letters)
    Condition
      Error in `bind_ard()`:
      ! The `.update` argument must be class <logical>.

---

    Code
      bind_ard(ard, ard, .update = FALSE)
    Condition
      Error in `bind_ard()`:
      ! 27 duplicate observations found.

# bind_ard() .order argument works

    Code
      dplyr::select(as.data.frame(bind_ard(ard_categorical(ADSL, by = "ARM",
        variables = c("SEX", "AGEGR1")), ard_chisqtest(ADSL, by = "ARM", variable = "AGEGR1"),
      ard_chisqtest(ADSL, by = "ARM", variable = "SEX"), .order = TRUE)), -c(context,
        statistic_fmt_fn, warning, error))
    Output
         group1         group1_level variable variable_level        stat_name
      1     ARM              Placebo      SEX              F                n
      2     ARM              Placebo      SEX              F                N
      3     ARM              Placebo      SEX              F                p
      4     ARM              Placebo      SEX              M                n
      5     ARM              Placebo      SEX              M                N
      6     ARM              Placebo      SEX              M                p
      7     ARM Xanomeline High Dose      SEX              F                n
      8     ARM Xanomeline High Dose      SEX              F                N
      9     ARM Xanomeline High Dose      SEX              F                p
      10    ARM Xanomeline High Dose      SEX              M                n
      11    ARM Xanomeline High Dose      SEX              M                N
      12    ARM Xanomeline High Dose      SEX              M                p
      13    ARM  Xanomeline Low Dose      SEX              F                n
      14    ARM  Xanomeline Low Dose      SEX              F                N
      15    ARM  Xanomeline Low Dose      SEX              F                p
      16    ARM  Xanomeline Low Dose      SEX              M                n
      17    ARM  Xanomeline Low Dose      SEX              M                N
      18    ARM  Xanomeline Low Dose      SEX              M                p
      19    ARM                 NULL      SEX           NULL        statistic
      20    ARM                 NULL      SEX           NULL          p.value
      21    ARM                 NULL      SEX           NULL        parameter
      22    ARM                 NULL      SEX           NULL           method
      23    ARM                 NULL      SEX           NULL          correct
      24    ARM                 NULL      SEX           NULL                p
      25    ARM                 NULL      SEX           NULL        rescale.p
      26    ARM                 NULL      SEX           NULL simulate.p.value
      27    ARM                 NULL      SEX           NULL                B
      28    ARM              Placebo   AGEGR1          65-80                n
      29    ARM              Placebo   AGEGR1          65-80                N
      30    ARM              Placebo   AGEGR1          65-80                p
      31    ARM              Placebo   AGEGR1            <65                n
      32    ARM              Placebo   AGEGR1            <65                N
      33    ARM              Placebo   AGEGR1            <65                p
      34    ARM              Placebo   AGEGR1            >80                n
      35    ARM              Placebo   AGEGR1            >80                N
      36    ARM              Placebo   AGEGR1            >80                p
      37    ARM Xanomeline High Dose   AGEGR1          65-80                n
      38    ARM Xanomeline High Dose   AGEGR1          65-80                N
      39    ARM Xanomeline High Dose   AGEGR1          65-80                p
      40    ARM Xanomeline High Dose   AGEGR1            <65                n
      41    ARM Xanomeline High Dose   AGEGR1            <65                N
      42    ARM Xanomeline High Dose   AGEGR1            <65                p
      43    ARM Xanomeline High Dose   AGEGR1            >80                n
      44    ARM Xanomeline High Dose   AGEGR1            >80                N
      45    ARM Xanomeline High Dose   AGEGR1            >80                p
      46    ARM  Xanomeline Low Dose   AGEGR1          65-80                n
      47    ARM  Xanomeline Low Dose   AGEGR1          65-80                N
      48    ARM  Xanomeline Low Dose   AGEGR1          65-80                p
      49    ARM  Xanomeline Low Dose   AGEGR1            <65                n
      50    ARM  Xanomeline Low Dose   AGEGR1            <65                N
      51    ARM  Xanomeline Low Dose   AGEGR1            <65                p
      52    ARM  Xanomeline Low Dose   AGEGR1            >80                n
      53    ARM  Xanomeline Low Dose   AGEGR1            >80                N
      54    ARM  Xanomeline Low Dose   AGEGR1            >80                p
      55    ARM                 NULL   AGEGR1           NULL        statistic
      56    ARM                 NULL   AGEGR1           NULL          p.value
      57    ARM                 NULL   AGEGR1           NULL        parameter
      58    ARM                 NULL   AGEGR1           NULL           method
      59    ARM                 NULL   AGEGR1           NULL          correct
      60    ARM                 NULL   AGEGR1           NULL                p
      61    ARM                 NULL   AGEGR1           NULL        rescale.p
      62    ARM                 NULL   AGEGR1           NULL simulate.p.value
      63    ARM                 NULL   AGEGR1           NULL                B
                  stat_label                   statistic
      1                    n                          53
      2                    N                          86
      3                    %                   0.6162791
      4                    n                          33
      5                    N                          86
      6                    %                   0.3837209
      7                    n                          40
      8                    N                          84
      9                    %                   0.4761905
      10                   n                          44
      11                   N                          84
      12                   %                   0.5238095
      13                   n                          50
      14                   N                          84
      15                   %                   0.5952381
      16                   n                          34
      17                   N                          84
      18                   %                   0.4047619
      19 X-squared Statistic                     3.91998
      20             p-value                   0.1408598
      21           parameter                           2
      22              method  Pearson's Chi-squared test
      23             correct                        TRUE
      24                   p rep(1/length(x), length(x))
      25           rescale.p                       FALSE
      26    simulate.p.value                       FALSE
      27                   B                        2000
      28                   n                          42
      29                   N                          86
      30                   %                   0.4883721
      31                   n                          14
      32                   N                          86
      33                   %                   0.1627907
      34                   n                          30
      35                   N                          86
      36                   %                   0.3488372
      37                   n                          55
      38                   N                          84
      39                   %                   0.6547619
      40                   n                          11
      41                   N                          84
      42                   %                   0.1309524
      43                   n                          18
      44                   N                          84
      45                   %                   0.2142857
      46                   n                          47
      47                   N                          84
      48                   %                   0.5595238
      49                   n                           8
      50                   N                          84
      51                   %                   0.0952381
      52                   n                          29
      53                   N                          84
      54                   %                   0.3452381
      55 X-squared Statistic                    6.852038
      56             p-value                    0.143917
      57           parameter                           4
      58              method  Pearson's Chi-squared test
      59             correct                        TRUE
      60                   p rep(1/length(x), length(x))
      61           rescale.p                       FALSE
      62    simulate.p.value                       FALSE
      63                   B                        2000

---

    Code
      dplyr::select(as.data.frame(bind_ard(ard_categorical(ADSL, by = "ARM",
        variables = c("SEX", "AGEGR1")), ard_chisqtest(ADSL, by = "ARM", variable = "AGEGR1"),
      ard_chisqtest(ADSL, by = "ARM", variable = "SEX"), .order = FALSE)), -c(context,
        statistic_fmt_fn, warning, error))
    Output
         group1         group1_level variable variable_level        stat_name
      1     ARM              Placebo      SEX              F                n
      2     ARM              Placebo      SEX              F                N
      3     ARM              Placebo      SEX              F                p
      4     ARM Xanomeline High Dose      SEX              F                n
      5     ARM Xanomeline High Dose      SEX              F                N
      6     ARM Xanomeline High Dose      SEX              F                p
      7     ARM  Xanomeline Low Dose      SEX              F                n
      8     ARM  Xanomeline Low Dose      SEX              F                N
      9     ARM  Xanomeline Low Dose      SEX              F                p
      10    ARM              Placebo      SEX              M                n
      11    ARM              Placebo      SEX              M                N
      12    ARM              Placebo      SEX              M                p
      13    ARM Xanomeline High Dose      SEX              M                n
      14    ARM Xanomeline High Dose      SEX              M                N
      15    ARM Xanomeline High Dose      SEX              M                p
      16    ARM  Xanomeline Low Dose      SEX              M                n
      17    ARM  Xanomeline Low Dose      SEX              M                N
      18    ARM  Xanomeline Low Dose      SEX              M                p
      19    ARM              Placebo   AGEGR1          65-80                n
      20    ARM              Placebo   AGEGR1          65-80                N
      21    ARM              Placebo   AGEGR1          65-80                p
      22    ARM Xanomeline High Dose   AGEGR1          65-80                n
      23    ARM Xanomeline High Dose   AGEGR1          65-80                N
      24    ARM Xanomeline High Dose   AGEGR1          65-80                p
      25    ARM  Xanomeline Low Dose   AGEGR1          65-80                n
      26    ARM  Xanomeline Low Dose   AGEGR1          65-80                N
      27    ARM  Xanomeline Low Dose   AGEGR1          65-80                p
      28    ARM              Placebo   AGEGR1            <65                n
      29    ARM              Placebo   AGEGR1            <65                N
      30    ARM              Placebo   AGEGR1            <65                p
      31    ARM Xanomeline High Dose   AGEGR1            <65                n
      32    ARM Xanomeline High Dose   AGEGR1            <65                N
      33    ARM Xanomeline High Dose   AGEGR1            <65                p
      34    ARM  Xanomeline Low Dose   AGEGR1            <65                n
      35    ARM  Xanomeline Low Dose   AGEGR1            <65                N
      36    ARM  Xanomeline Low Dose   AGEGR1            <65                p
      37    ARM              Placebo   AGEGR1            >80                n
      38    ARM              Placebo   AGEGR1            >80                N
      39    ARM              Placebo   AGEGR1            >80                p
      40    ARM Xanomeline High Dose   AGEGR1            >80                n
      41    ARM Xanomeline High Dose   AGEGR1            >80                N
      42    ARM Xanomeline High Dose   AGEGR1            >80                p
      43    ARM  Xanomeline Low Dose   AGEGR1            >80                n
      44    ARM  Xanomeline Low Dose   AGEGR1            >80                N
      45    ARM  Xanomeline Low Dose   AGEGR1            >80                p
      46    ARM                 NULL   AGEGR1           NULL        statistic
      47    ARM                 NULL   AGEGR1           NULL          p.value
      48    ARM                 NULL   AGEGR1           NULL        parameter
      49    ARM                 NULL   AGEGR1           NULL           method
      50    ARM                 NULL   AGEGR1           NULL          correct
      51    ARM                 NULL   AGEGR1           NULL                p
      52    ARM                 NULL   AGEGR1           NULL        rescale.p
      53    ARM                 NULL   AGEGR1           NULL simulate.p.value
      54    ARM                 NULL   AGEGR1           NULL                B
      55    ARM                 NULL      SEX           NULL        statistic
      56    ARM                 NULL      SEX           NULL          p.value
      57    ARM                 NULL      SEX           NULL        parameter
      58    ARM                 NULL      SEX           NULL           method
      59    ARM                 NULL      SEX           NULL          correct
      60    ARM                 NULL      SEX           NULL                p
      61    ARM                 NULL      SEX           NULL        rescale.p
      62    ARM                 NULL      SEX           NULL simulate.p.value
      63    ARM                 NULL      SEX           NULL                B
                  stat_label                   statistic
      1                    n                          53
      2                    N                          86
      3                    %                   0.6162791
      4                    n                          40
      5                    N                          84
      6                    %                   0.4761905
      7                    n                          50
      8                    N                          84
      9                    %                   0.5952381
      10                   n                          33
      11                   N                          86
      12                   %                   0.3837209
      13                   n                          44
      14                   N                          84
      15                   %                   0.5238095
      16                   n                          34
      17                   N                          84
      18                   %                   0.4047619
      19                   n                          42
      20                   N                          86
      21                   %                   0.4883721
      22                   n                          55
      23                   N                          84
      24                   %                   0.6547619
      25                   n                          47
      26                   N                          84
      27                   %                   0.5595238
      28                   n                          14
      29                   N                          86
      30                   %                   0.1627907
      31                   n                          11
      32                   N                          84
      33                   %                   0.1309524
      34                   n                           8
      35                   N                          84
      36                   %                   0.0952381
      37                   n                          30
      38                   N                          86
      39                   %                   0.3488372
      40                   n                          18
      41                   N                          84
      42                   %                   0.2142857
      43                   n                          29
      44                   N                          84
      45                   %                   0.3452381
      46 X-squared Statistic                    6.852038
      47             p-value                    0.143917
      48           parameter                           4
      49              method  Pearson's Chi-squared test
      50             correct                        TRUE
      51                   p rep(1/length(x), length(x))
      52           rescale.p                       FALSE
      53    simulate.p.value                       FALSE
      54                   B                        2000
      55 X-squared Statistic                     3.91998
      56             p-value                   0.1408598
      57           parameter                           2
      58              method  Pearson's Chi-squared test
      59             correct                        TRUE
      60                   p rep(1/length(x), length(x))
      61           rescale.p                       FALSE
      62    simulate.p.value                       FALSE
      63                   B                        2000

