# mock_categorical()

    Code
      out
    Message
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level stat_name stat_label stat
      1    TRTA      Placebo   AGEGR1            <65         n          n     
      2    TRTA      Placebo   AGEGR1            <65         p          %     
      3    TRTA      Placebo   AGEGR1            <65         N          N     
      4    TRTA      Placebo   AGEGR1          65-80         n          n     
      5    TRTA      Placebo   AGEGR1          65-80         p          %     
      6    TRTA      Placebo   AGEGR1          65-80         N          N     
      7    TRTA      Placebo   AGEGR1            >80         n          n     
      8    TRTA      Placebo   AGEGR1            >80         p          %     
      9    TRTA      Placebo   AGEGR1            >80         N          N     
      10   TRTA    Xanomeli…   AGEGR1            <65         n          n     
    Message
      i 17 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fun, warning, error

# mock_categorical() messaging

    Code
      mock_categorical(variables = list(AGEGR1 = factor(c("<65", "65-80", ">80"),
      levels = c("<65", "65-80", ">80"))), statistic = ~ c("NOTASTATISTIC"))
    Condition
      Error in `mock_categorical()`:
      ! The elements of the `statistic` argument must be vector with one or more of "n", "p", and "N".

# mock_continuous()

    Code
      out
    Message
      {cards} data frame: 16 x 8
    Output
         variable   context stat_name stat_label stat fmt_fun
      1       AGE continuo…         N          N         <fn>
      2       AGE continuo…      mean       Mean         <fn>
      3       AGE continuo…        sd         SD         <fn>
      4       AGE continuo…    median     Median         <fn>
      5       AGE continuo…       p25         Q1         <fn>
      6       AGE continuo…       p75         Q3         <fn>
      7       AGE continuo…       min        Min         <fn>
      8       AGE continuo…       max        Max         <fn>
      9     BMIBL continuo…         N          N         <fn>
      10    BMIBL continuo…      mean       Mean         <fn>
      11    BMIBL continuo…        sd         SD         <fn>
      12    BMIBL continuo…    median     Median         <fn>
      13    BMIBL continuo…       p25         Q1         <fn>
      14    BMIBL continuo…       p75         Q3         <fn>
      15    BMIBL continuo…       min        Min         <fn>
      16    BMIBL continuo…       max        Max         <fn>
    Message
      i 2 more variables: warning, error

# mock_continuous() messaging

    Code
      mock_continuous(variables = c("AGE", "BMIBL"), statistic = ~t.test)
    Condition
      Error in `mock_continuous()`:
      ! The elements of the `statistic` argument must be <character> vector of statistic names.

# mock_dichotomous()

    Code
      out
    Message
      {cards} data frame: 9 x 11
    Output
        group1 group1_level variable variable_level stat_name stat_label stat
      1   TRTA      Placebo   AGEGR1          65-80         n          n     
      2   TRTA      Placebo   AGEGR1          65-80         p          %     
      3   TRTA      Placebo   AGEGR1          65-80         N          N     
      4   TRTA    Xanomeli…   AGEGR1          65-80         n          n     
      5   TRTA    Xanomeli…   AGEGR1          65-80         p          %     
      6   TRTA    Xanomeli…   AGEGR1          65-80         N          N     
      7   TRTA    Xanomeli…   AGEGR1          65-80         n          n     
      8   TRTA    Xanomeli…   AGEGR1          65-80         p          %     
      9   TRTA    Xanomeli…   AGEGR1          65-80         N          N     
    Message
      i 4 more variables: context, fmt_fun, warning, error

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
      out
    Message
      {cards} data frame: 10 x 8
    Output
         variable context stat_name stat_label stat fmt_fun
      1       AGE missing     N_obs  Vector L…         <fn>
      2       AGE missing    N_miss  N Missing         <fn>
      3       AGE missing N_nonmiss  N Non-mi…         <fn>
      4       AGE missing    p_miss  % Missing         <fn>
      5       AGE missing p_nonmiss  % Non-mi…         <fn>
      6     BMIBL missing     N_obs  Vector L…         <fn>
      7     BMIBL missing    N_miss  N Missing         <fn>
      8     BMIBL missing N_nonmiss  N Non-mi…         <fn>
      9     BMIBL missing    p_miss  % Missing         <fn>
      10    BMIBL missing p_nonmiss  % Non-mi…         <fn>
    Message
      i 2 more variables: warning, error

# mock_missing() messaging

    Code
      mock_missing(variables = c("AGE", "BMIBL"), statistic = ~letters)
    Condition
      Error in `mock_missing()`:
      ! The elements of the `statistic` argument must be vector with one or more of "N_obs", "N_miss", "N_nonmiss", "p_miss", and "p_nonmiss".

# mock_attributes()

    Code
      out
    Message
      {cards} data frame: 4 x 8
    Output
        variable   context stat_name stat_label      stat fmt_fun
      1      AGE attribut…     label  Variable…       Age    <fn>
      2      AGE attribut…     class  Variable…   logical    NULL
      3    BMIBL attribut…     label  Variable… Baseline…    <fn>
      4    BMIBL attribut…     class  Variable…   logical    NULL
    Message
      i 2 more variables: warning, error

# mock_attributes() messaging

    Code
      mock_attributes(label = c("AGE", "BMIBL"))
    Condition
      Error in `mock_attributes()`:
      ! The `label` argument must be a named list.

# mock_total_n()

    Code
      out
    Message
      {cards} data frame: 1 x 8
    Output
               variable context stat_name stat_label stat fmt_fun
      1 ..ard_total_n.. total_n         N          N         <fn>
    Message
      i 2 more variables: warning, error

