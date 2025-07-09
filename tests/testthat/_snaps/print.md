# print.card() works

    Code
      ard_continuous(ADSL, by = "ARM", variables = "AGE")
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
         group1 group1_level variable stat_name stat_label stat  
      1  ARM    Placebo      AGE      N         N          86    
      2  ARM    Placebo      AGE      mean      Mean       75.209
      3  ARM    Placebo      AGE      sd        SD         8.59  
      4  ARM    Placebo      AGE      median    Median     76    
      5  ARM    Placebo      AGE      p25       Q1         69    
      6  ARM    Placebo      AGE      p75       Q3         82    
      7  ARM    Placebo      AGE      min       Min        52    
      8  ARM    Placebo      AGE      max       Max        89    
      9  ARM    Xanomeli…    AGE      N         N          84    
      10 ARM    Xanomeli…    AGE      mean      Mean       74.381
    Message
      i Showing 10 of 24 rows.

---

    Code
      ard_categorical(ADSL, by = "ARM", variables = "AGEGR1")
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
         group1 group1_level variable variable_level stat_name stat_label stat 
      1  ARM    Placebo      AGEGR1   65-80          n         n          42   
      2  ARM    Placebo      AGEGR1   65-80          N         N          86   
      3  ARM    Placebo      AGEGR1   65-80          p         %          0.488
      4  ARM    Placebo      AGEGR1   <65            n         n          14   
      5  ARM    Placebo      AGEGR1   <65            N         N          86   
      6  ARM    Placebo      AGEGR1   <65            p         %          0.163
      7  ARM    Placebo      AGEGR1   >80            n         n          30   
      8  ARM    Placebo      AGEGR1   >80            N         N          86   
      9  ARM    Placebo      AGEGR1   >80            p         %          0.349
      10 ARM    Xanomeli…    AGEGR1   65-80          n         n          55   
    Message
      i Showing 10 of 27 rows.

---

    Code
      ard_continuous(ADSL, variables = "AGE", fmt_fun = AGE ~ list(~ function(x)
        round(x, 3)))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
        variable context   stat_name stat_label stat   fmt_fun                   
      1 AGE      continuo… N         N          254    function (x) , round(x, 3)
      2 AGE      continuo… mean      Mean       75.087 function (x) , round(x, 3)
      3 AGE      continuo… sd        SD         8.246  function (x) , round(x, 3)
      4 AGE      continuo… median    Median     77     function (x) , round(x, 3)
      5 AGE      continuo… p25       Q1         70     function (x) , round(x, 3)
      6 AGE      continuo… p75       Q3         81     function (x) , round(x, 3)
      7 AGE      continuo… min       Min        51     function (x) , round(x, 3)
      8 AGE      continuo… max       Max        89     function (x) , round(x, 3)

---

    Code
      dplyr::select(ard_continuous(data = data.frame(x = seq(as.Date("2000-01-01"),
      length.out = 10L, by = "day")), variables = x, statistic = ~
      continuous_summary_fns(c("min", "max", "sd"))), -fmt_fun)
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
        variable context   stat_name stat_label stat  error
      1 x        continuo… min       Min        10957      
      2 x        continuo… max       Max        10966      
      3 x        continuo… sd        SD         3.028      

---

    Code
      bind_ard(ard_attributes(mtcars, variables = mpg), ard_continuous(mtcars,
        variables = mpg, statistic = ~ continuous_summary_fns("mean", other_stats = list(
          vcov = function(x) vcov(lm(mpg ~ am, mtcars))))))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
        variable context   stat_name stat_label stat                        
      1 mpg      attribut… label     Variable…  mpg                         
      2 mpg      attribut… class     Variable…  numeric                     
      3 mpg      continuo… mean      Mean       20.091                      
      4 mpg      continuo… vcov      vcov       1.265, -1.265, -1.265, 3.113
        fmt_fun                   
      1 .Primitive("as.character")
      2 NULL                      
      3 1                         
      4 1                         

