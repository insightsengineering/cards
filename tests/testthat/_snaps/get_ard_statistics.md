# get_ard_statistics() works

    Code
      get_ard_statistics(ard, group1_level %in% "Placebo", variable_level %in%
      "65-80")
    Output
      $n
      [1] 42
      
      $N
      [1] 86
      
      $p
      [1] 0.4883721
      

---

    Code
      get_ard_statistics(ard, group1_level %in% "Placebo", variable_level %in%
      "65-80", .attributes = c("warning", "error"))
    Output
      $n
      [1] 42
      attr(,"warning")
      [1] "ARM"
      attr(,"error")
      [1] "Placebo"
      
      $N
      [1] 86
      attr(,"warning")
      [1] "ARM"
      attr(,"error")
      [1] "Placebo"
      
      $p
      [1] 0.4883721
      attr(,"warning")
      [1] "ARM"
      attr(,"error")
      [1] "Placebo"
      

