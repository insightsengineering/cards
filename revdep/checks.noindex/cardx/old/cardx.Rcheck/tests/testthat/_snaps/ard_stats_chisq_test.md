# shuffle_ard fills missing group levels if the group is meaningful

    Code
      as.data.frame(cards::shuffle_ard(cards::bind_ard(ard_stats_chisq_test(data = adsl_sub,
        by = "ARM", variables = "AGEGR1"), ard_stats_chisq_test(data = adsl_sub, by = "SEX",
        variables = "AGEGR1"))))
    Output
                ARM         SEX variable          context stat_name         stat
      1 Overall ARM        <NA>   AGEGR1 stats_chisq_test statistic 5.079442e+00
      2 Overall ARM        <NA>   AGEGR1 stats_chisq_test   p.value 7.888842e-02
      3 Overall ARM        <NA>   AGEGR1 stats_chisq_test parameter 2.000000e+00
      4 Overall ARM        <NA>   AGEGR1 stats_chisq_test         B 2.000000e+03
      5        <NA> Overall SEX   AGEGR1 stats_chisq_test statistic 1.039442e+00
      6        <NA> Overall SEX   AGEGR1 stats_chisq_test   p.value 5.946864e-01
      7        <NA> Overall SEX   AGEGR1 stats_chisq_test parameter 2.000000e+00
      8        <NA> Overall SEX   AGEGR1 stats_chisq_test         B 2.000000e+03

