# ARD helpers works

    Code
      get_ard_statistics(ard, group1_level %in% "Placebo", variable_level %in%
      "65-80")
    Output
      $n
      [1] 42
      attr(,"stat_label")
      [1] "n"
      attr(,"statistic_fmt_fn")
      function(x) format(round(x, digits = 0), nsmall = 0)
      <environment: 0x106aed200>
      
      $p
      [1] 0.4883721
      attr(,"stat_label")
      [1] "%"
      attr(,"statistic_fmt_fn")
      function(x) format(round(x, digits = 0), nsmall = 0)
      <environment: 0x106aed200>
      

