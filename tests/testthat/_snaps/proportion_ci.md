# check the proportion_ci_*() functions work

    Code
      wilson_dbl <- proportion_ci_wilson(x_dbl, conf.level = 0.9, correct = FALSE)

---

    Code
      wilsoncc_dbl <- proportion_ci_wilson(x_dbl, conf.level = 0.9, correct = TRUE)

---

    Code
      wilson_lgl <- proportion_ci_wilson(x_lgl, conf.level = 0.9, correct = FALSE)

---

    Code
      proportion_ci_wilson(x_rsp, conf.level = 0.9)
    Output
      $N
      [1] 10
      
      $conf.level
      [1] 0.9
      
      $estimate
        p 
      0.5 
      
      $statistic
      X-squared 
              0 
      
      $p.value
      [1] 1
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0.2692718
      
      $conf.high
      [1] 0.7307282
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      

---

    Code
      proportion_ci_wilson(x_true)
    Output
      $N
      [1] 32
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      1 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0.8928208
      
      $conf.high
      [1] 1
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      

---

    Code
      proportion_ci_wilson(x_false)
    Output
      $N
      [1] 32
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      0 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.1071792
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      

---

    Code
      wald_dbl <- proportion_ci_wald(x_dbl, conf.level = 0.9, correct = FALSE)

---

    Code
      waldcc_dbl <- proportion_ci_wald(x_dbl, conf.level = 0.9, correct = TRUE)

---

    Code
      wald_lgl <- proportion_ci_wald(x_lgl, conf.level = 0.9, correct = FALSE)

---

    Code
      proportion_ci_wald(x_rsp, conf.level = 0.95, correct = TRUE)
    Output
      $N
      [1] 10
      
      $estimate
      [1] 0.5
      
      $conf.low
      [1] 0.1401025
      
      $conf.high
      [1] 0.8598975
      
      $conf.level
      [1] 0.95
      
      $method
      Wald Confidence Interval with continuity correction
      

---

    Code
      proportion_ci_wald(x_true)
    Output
      $N
      [1] 32
      
      $estimate
      [1] 1
      
      $conf.low
      [1] 1
      
      $conf.high
      [1] 1
      
      $conf.level
      [1] 0.95
      
      $method
      Wald Confidence Interval without continuity correction
      

---

    Code
      proportion_ci_wald(x_false)
    Output
      $N
      [1] 32
      
      $estimate
      [1] 0
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0
      
      $conf.level
      [1] 0.95
      
      $method
      Wald Confidence Interval without continuity correction
      

---

    Code
      clopper_pearson_dbl <- proportion_ci_clopper_pearson(x_dbl, conf.level = 0.9)

---

    Code
      clopper_pearson_lgl <- proportion_ci_clopper_pearson(x_lgl, conf.level = 0.9)

---

    Code
      proportion_ci_clopper_pearson(x_rsp, conf.level = 0.95)
    Output
      $N
      [1] 10
      
      $conf.level
      [1] 0.95
      
      $estimate
      probability of success 
                         0.5 
      
      $statistic
      number of successes 
                        5 
      
      $p.value
      [1] 1
      
      $parameter
      number of trials 
                    10 
      
      $conf.low
      [1] 0.187086
      
      $conf.high
      [1] 0.812914
      
      $method
      [1] "Clopper-Pearson Confidence Interval"
      
      $alternative
      [1] "two.sided"
      

---

    Code
      proportion_ci_wilson(x_true)
    Output
      $N
      [1] 32
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      1 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0.8928208
      
      $conf.high
      [1] 1
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      

---

    Code
      proportion_ci_wilson(x_false)
    Output
      $N
      [1] 32
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      0 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.1071792
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      

---

    Code
      agresti_coull_dbl <- proportion_ci_agresti_coull(x_dbl, conf.level = 0.9)

---

    Code
      agresti_coull_lgl <- proportion_ci_agresti_coull(x_lgl, conf.level = 0.9)

---

    Code
      proportion_ci_agresti_coull(x_rsp, conf.level = 0.95)
    Output
      $N
      [1] 10
      
      $estimate
      [1] 0.5
      
      $conf.low
      [1] 0.2365931
      
      $conf.high
      [1] 0.7634069
      
      $conf.level
      [1] 0.95
      
      $method
      [1] "Agresti-Coull Confidence Interval"
      

---

    Code
      proportion_ci_agresti_coull(x_true)
    Output
      $N
      [1] 32
      
      $estimate
      [1] 1
      
      $conf.low
      [1] 0.8726819
      
      $conf.high
      [1] 1
      
      $conf.level
      [1] 0.95
      
      $method
      [1] "Agresti-Coull Confidence Interval"
      

---

    Code
      proportion_ci_agresti_coull(x_false)
    Output
      $N
      [1] 32
      
      $estimate
      [1] 0
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.1273181
      
      $conf.level
      [1] 0.95
      
      $method
      [1] "Agresti-Coull Confidence Interval"
      

---

    Code
      jeffreys_dbl <- proportion_ci_jeffreys(x_dbl, conf.level = 0.9)

---

    Code
      jeffreys_lgl <- proportion_ci_jeffreys(x_lgl, conf.level = 0.9)

---

    Code
      proportion_ci_jeffreys(x_rsp, conf.level = 0.95)
    Output
      $N
      [1] 10
      
      $estimate
      [1] 0.5
      
      $conf.low
      [1] 0.2235287
      
      $conf.high
      [1] 0.7764713
      
      $conf.level
      [1] 0.95
      
      $method
      Jeffreys Interval
      

---

    Code
      proportion_ci_jeffreys(x_true)
    Output
      $N
      [1] 32
      
      $estimate
      [1] 1
      
      $conf.low
      [1] 0.9250722
      
      $conf.high
      [1] 1
      
      $conf.level
      [1] 0.95
      
      $method
      Jeffreys Interval
      

---

    Code
      proportion_ci_jeffreys(x_false)
    Output
      $N
      [1] 32
      
      $estimate
      [1] 0
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.07492776
      
      $conf.level
      [1] 0.95
      
      $method
      Jeffreys Interval
      

# check the proportion_ci_strat_wilson() function works

    Code
      proportion_ci_strat_wilson(x = rsp, strata = strata, weights = weights,
        correct = FALSE)
    Output
      $N
      [1] 80
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.4867191
      
      $conf.high
      [1] 0.7186381
      
      $conf.level
      [1] 0.95
      
      $method
      Stratified Wilson Confidence Interval without continuity correction
      

---

    Code
      proportion_ci_strat_wilson(x = rsp, strata = strata, weights = weights,
        correct = TRUE)
    Output
      $N
      [1] 80
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.4482566
      
      $conf.high
      [1] 0.7531474
      
      $conf.level
      [1] 0.95
      
      $method
      Stratified Wilson Confidence Interval with continuity correction
      

---

    Code
      proportion_ci_strat_wilson(x = as.numeric(rsp), strata = strata, weights = weights)
    Output
      $N
      [1] 80
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.4867191
      
      $conf.high
      [1] 0.7186381
      
      $conf.level
      [1] 0.95
      
      $method
      Stratified Wilson Confidence Interval without continuity correction
      

---

    Code
      proportion_ci_strat_wilson(x = rep_len(TRUE, length(rsp)), strata = strata,
      weights = weights)
    Condition
      Error in `proportion_ci_strat_wilson()`:
      ! All values in `x` argument are either `TRUE` or `FALSE` and CI is not estimable.

---

    Code
      proportion_ci_strat_wilson(x = rep_len(FALSE, length(rsp)), strata = strata,
      weights = weights)
    Condition
      Error in `proportion_ci_strat_wilson()`:
      ! All values in `x` argument are either `TRUE` or `FALSE` and CI is not estimable.

