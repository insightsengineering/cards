# mock_categorical()

    Code
      apply_fmt_fun(mock_categorical(variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"), levels = c("<65", "65-80", ">80"))),
      by = list(TRTA = c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
         group1 group1_level variable variable_level stat_name stat_label stat stat_fmt
      1  TRTA   Placebo      AGEGR1   <65            n         n          NULL xx      
      2  TRTA   Placebo      AGEGR1   <65            p         %          NULL xx.x    
      3  TRTA   Placebo      AGEGR1   <65            N         N          NULL xx      
      4  TRTA   Placebo      AGEGR1   65-80          n         n          NULL xx      
      5  TRTA   Placebo      AGEGR1   65-80          p         %          NULL xx.x    
      6  TRTA   Placebo      AGEGR1   65-80          N         N          NULL xx      
      7  TRTA   Placebo      AGEGR1   >80            n         n          NULL xx      
      8  TRTA   Placebo      AGEGR1   >80            p         %          NULL xx.x    
      9  TRTA   Placebo      AGEGR1   >80            N         N          NULL xx      
      10 TRTA   Xanomeli…    AGEGR1   <65            n         n          NULL xx      
    Message
      i Showing 10 of 27 rows.

# mock_categorical() messaging

    Code
      mock_categorical(variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"),
      levels = c("<65", "65-80", ">80"))), statistic = ~ c("NOTASTATISTIC"))
    Condition
      Error in `mock_categorical()`:
      ! The elements of the `statistic` argument must be vector with one or more of "n", "p", and "N".

# mock_continuous()

    Code
      apply_fmt_fun(mock_continuous(variables = c("AGE", "BMIBL")))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
         variable context   stat_name stat_label stat stat_fmt
      1  AGE      continuo… N         N          NULL xx      
      2  AGE      continuo… mean      Mean       NULL xx.x    
      3  AGE      continuo… sd        SD         NULL xx.x    
      4  AGE      continuo… median    Median     NULL xx.x    
      5  AGE      continuo… p25       Q1         NULL xx.x    
      6  AGE      continuo… p75       Q3         NULL xx.x    
      7  AGE      continuo… min       Min        NULL xx.x    
      8  AGE      continuo… max       Max        NULL xx.x    
      9  BMIBL    continuo… N         N          NULL xx      
      10 BMIBL    continuo… mean      Mean       NULL xx.x    
      11 BMIBL    continuo… sd        SD         NULL xx.x    
      12 BMIBL    continuo… median    Median     NULL xx.x    
      13 BMIBL    continuo… p25       Q1         NULL xx.x    
      14 BMIBL    continuo… p75       Q3         NULL xx.x    
      15 BMIBL    continuo… min       Min        NULL xx.x    
      16 BMIBL    continuo… max       Max        NULL xx.x    

# mock_continuous() messaging

    Code
      mock_continuous(variables = c("AGE", "BMIBL"), statistic = ~t.test)
    Condition
      Error in `mock_continuous()`:
      ! The elements of the `statistic` argument must be <character> vector of statistic names.

# mock_dichotomous()

    Code
      apply_fmt_fun(mock_dichotomous(variables = list(AGEGR1 = factor("65-80", levels = c("<65", "65-80", ">80"))), by = list(TRTA = c(
        "Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
        group1 group1_level variable variable_level stat_name stat_label stat stat_fmt
      1 TRTA   Placebo      AGEGR1   65-80          n         n          NULL xx      
      2 TRTA   Placebo      AGEGR1   65-80          p         %          NULL xx.x    
      3 TRTA   Placebo      AGEGR1   65-80          N         N          NULL xx      
      4 TRTA   Xanomeli…    AGEGR1   65-80          n         n          NULL xx      
      5 TRTA   Xanomeli…    AGEGR1   65-80          p         %          NULL xx.x    
      6 TRTA   Xanomeli…    AGEGR1   65-80          N         N          NULL xx      
      7 TRTA   Xanomeli…    AGEGR1   65-80          n         n          NULL xx      
      8 TRTA   Xanomeli…    AGEGR1   65-80          p         %          NULL xx.x    
      9 TRTA   Xanomeli…    AGEGR1   65-80          N         N          NULL xx      

# mock_dichotomous() messaging

    Code
      mock_dichotomous(variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"),
      levels = c("<65", "65-80", ">80"))), by = list(TRTA = c("Placebo",
        "Xanomeline High Dose", "Xanomeline Low Dose")))
    Condition
      Error in `mock_dichotomous()`:
      ! The list values of `variables` argument must be length 1.

# mock_missing()

    Code
      apply_fmt_fun(mock_missing(variables = c("AGE", "BMIBL")))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
         variable context stat_name stat_label stat stat_fmt
      1  AGE      missing N_obs     Vector L…  NULL xx      
      2  AGE      missing N_miss    N Missing  NULL xx      
      3  AGE      missing N_nonmiss N Non-mi…  NULL xx      
      4  AGE      missing p_miss    % Missing  NULL xx.x    
      5  AGE      missing p_nonmiss % Non-mi…  NULL xx.x    
      6  BMIBL    missing N_obs     Vector L…  NULL xx      
      7  BMIBL    missing N_miss    N Missing  NULL xx      
      8  BMIBL    missing N_nonmiss N Non-mi…  NULL xx      
      9  BMIBL    missing p_miss    % Missing  NULL xx.x    
      10 BMIBL    missing p_nonmiss % Non-mi…  NULL xx.x    

# mock_missing() messaging

    Code
      mock_missing(variables = c("AGE", "BMIBL"), statistic = ~letters)
    Condition
      Error in `mock_missing()`:
      ! The elements of the `statistic` argument must be vector with one or more of "N_obs", "N_miss", "N_nonmiss", "p_miss", and "p_nonmiss".

# mock_attributes()

    Code
      mock_attributes(label = list(AGE = "Age", BMIBL = "Baseline BMI"))
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
        variable context   stat_name stat_label stat         fmt_fun                   
      1 AGE      attribut… label     Variable…  Age          .Primitive("as.character")
      2 AGE      attribut… class     Variable…  logical      NULL                      
      3 BMIBL    attribut… label     Variable…  Baseline BMI .Primitive("as.character")
      4 BMIBL    attribut… class     Variable…  logical      NULL                      

# mock_attributes() messaging

    Code
      mock_attributes(label = c("AGE", "BMIBL"))
    Condition
      Error in `mock_attributes()`:
      ! The `label` argument must be a named list.

# mock_total_n()

    Code
      apply_fmt_fun(mock_total_n())
    Message
      
      -- cards -----------------------------------------------------------------------
    Output
        variable        context stat_name stat_label stat stat_fmt
      1 ..ard_total_n.. total_n N         N          NULL xx      

