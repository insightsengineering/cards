# shuffle/trim works

    Code
      ard_simple_shuffled
    Output
        variable    context stat_name stat_label       stat fmt_fun warning error
      1      AGE continuous         N          N 254.000000       0    NULL  NULL
      2      AGE continuous      mean       Mean  75.086614       1    NULL  NULL
      3      AGE continuous        sd         SD   8.246234       1    NULL  NULL
      4      AGE continuous    median     Median  77.000000       1    NULL  NULL
      5      AGE continuous       p25         Q1  70.000000       1    NULL  NULL
      6      AGE continuous       p75         Q3  81.000000       1    NULL  NULL
      7      AGE continuous       min        Min  51.000000       1    NULL  NULL
      8      AGE continuous       max        Max  89.000000       1    NULL  NULL

---

    Code
      ard_shuffled[1:5, ]
    Output
                         ARM variable       variable_level     context stat_name stat_label        stat
      1              Placebo      ARM              Placebo categorical         n          n  86.0000000
      2              Placebo      ARM              Placebo categorical         N          N 254.0000000
      3              Placebo      ARM              Placebo categorical         p          %   0.3385827
      4 Xanomeline High Dose      ARM Xanomeline High Dose categorical         n          n  84.0000000
      5 Xanomeline High Dose      ARM Xanomeline High Dose categorical         N          N 254.0000000

---

    Code
      ard_shuff_trim[1:5, ]
    Output
                         ARM variable       variable_level     context stat_name stat_label        stat
      1              Placebo      ARM              Placebo categorical         n          n  86.0000000
      2              Placebo      ARM              Placebo categorical         N          N 254.0000000
      3              Placebo      ARM              Placebo categorical         p          %   0.3385827
      4 Xanomeline High Dose      ARM Xanomeline High Dose categorical         n          n  84.0000000
      5 Xanomeline High Dose      ARM Xanomeline High Dose categorical         N          N 254.0000000

# shuffle_ard notifies user about warnings/errors before dropping

    Code
      shuffle_ard(ard_continuous(ADSL, variables = AGEGR1))
    Message
      "warning" column contains messages that will be removed.
    Output
      # A tibble: 8 x 5
        variable context    stat_name stat_label stat 
        <chr>    <chr>      <chr>     <chr>      <chr>
      1 AGEGR1   continuous N         N          254  
      2 AGEGR1   continuous mean      Mean       <NA> 
      3 AGEGR1   continuous sd        SD         <NA> 
      4 AGEGR1   continuous median    Median     <NA> 
      5 AGEGR1   continuous p25       Q1         65-80
      6 AGEGR1   continuous p75       Q3         >80  
      7 AGEGR1   continuous min       Min        65-80
      8 AGEGR1   continuous max       Max        >80  

# shuffle_ard fills missing group levels if the group is meaningful

    Code
      shuffle_ard(dplyr::filter(bind_ard(ard_continuous(ADSL, by = "ARM", variables = "AGE", statistic = ~ continuous_summary_fns("mean")), dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p",
        stat_label = "p", stat = list(0.05))), dplyr::row_number() <= 5L))
    Output
      # A tibble: 4 x 6
        ARM                  variable context    stat_name stat_label  stat
        <chr>                <chr>    <chr>      <chr>     <chr>      <dbl>
      1 Placebo              AGE      continuous mean      Mean       75.2 
      2 Xanomeline High Dose AGE      continuous mean      Mean       74.4 
      3 Xanomeline Low Dose  AGE      continuous mean      Mean       75.7 
      4 Overall ARM          AGE      <NA>       p         p           0.05

---

    Code
      shuffle_ard(dplyr::filter(bind_ard(ard_continuous(ADSL, variables = "AGE", statistic = ~ continuous_summary_fns("mean")), dplyr::tibble(group1 = "ARM", variable = "AGE", stat_name = "p", stat_label = "p",
        stat = list(0.05))), dplyr::row_number() <= 5L))
    Output
      # A tibble: 2 x 6
        ARM         variable context    stat_name stat_label  stat
        <chr>       <chr>    <chr>      <chr>     <chr>      <dbl>
      1 <NA>        AGE      continuous mean      Mean       75.1 
      2 Overall ARM AGE      <NA>       p         p           0.05

---

    Code
      as.data.frame(shuffle_ard(bind_ard(dplyr::slice(ard_categorical(ADSL, by = ARM, variables = AGEGR1), 1), dplyr::slice(ard_categorical(ADSL, variables = AGEGR1), 1), dplyr::slice(ard_continuous(ADSL,
        by = SEX, variables = AGE), 1), dplyr::slice(ard_continuous(ADSL, variables = AGE), 1))))
    Output
                ARM         SEX variable variable_level     context stat_name stat_label stat
      1     Placebo        <NA>   AGEGR1          65-80 categorical         n          n   42
      2 Overall ARM        <NA>   AGEGR1          65-80 categorical         n          n  144
      3        <NA> Overall SEX      AGE           <NA>  continuous         N          N  254
      4        <NA>           F      AGE           <NA>  continuous         N          N  143

---

    Code
      shuffle_ard(bind_ard(dplyr::slice(ard_categorical(ADSL, by = c(ARM, SEX), variables = AGEGR1), 1), dplyr::slice(ard_categorical(ADSL, by = SEX, variables = AGEGR1), 1), dplyr::slice(ard_categorical(
        ADSL, variables = AGEGR1), 1)))
    Output
      # A tibble: 3 x 8
        ARM         SEX         variable variable_level context     stat_name stat_label  stat
        <chr>       <chr>       <chr>    <chr>          <chr>       <chr>     <chr>      <int>
      1 Placebo     F           AGEGR1   65-80          categorical n         n             22
      2 Overall ARM F           AGEGR1   65-80          categorical n         n             78
      3 Overall ARM Overall SEX AGEGR1   65-80          categorical n         n            144

---

    Code
      shuffle_ard(bind_ard(ard_continuous(adsl_new, variables = "AGE", statistic = ~ continuous_summary_fns("mean")), ard_continuous(adsl_new, by = "ARM", variables = "AGE", statistic = ~
        continuous_summary_fns("mean"))))
    Output
      # A tibble: 4 x 6
        ARM                  variable context    stat_name stat_label  stat
        <chr>                <chr>    <chr>      <chr>     <chr>      <dbl>
      1 Overall ARM.1        AGE      continuous mean      Mean        75.1
      2 Overall ARM          AGE      continuous mean      Mean        75.2
      3 Xanomeline High Dose AGE      continuous mean      Mean        74.4
      4 Xanomeline Low Dose  AGE      continuous mean      Mean        75.7

# shuffle_ard fills missing group levels if the group is meaningful for cardx output

    Code
      as.data.frame(shuffle_ard(ard_cardx))
    Output
                ARM         SEX variable          context stat_name          stat_label       stat
      1 Overall ARM        <NA>   AGEGR1 stats_chisq_test statistic X-squared Statistic 5.07944167
      2 Overall ARM        <NA>   AGEGR1 stats_chisq_test   p.value             p-value 0.07888842
      3        <NA> Overall SEX   AGEGR1 stats_chisq_test statistic X-squared Statistic 1.03944200
      4        <NA> Overall SEX   AGEGR1 stats_chisq_test   p.value             p-value 0.59468644

