# tidy_as_ard() works / with standard use

    Code
      ard
    Message
      {cards} data frame: 12 x 8
    Output
         group1 variable   context        stat_name        stat fmt_fun
      1      am       vs fisherte…         estimate       1.956       1
      2      am       vs fisherte…          p.value       0.473       1
      3      am       vs fisherte…           method   Fisher's…    NULL
      4      am       vs fisherte…        workspace       2e+05       1
      5      am       vs fisherte…           hybrid       FALSE    NULL
      6      am       vs fisherte…       hybridPars c, 5, 80, 1    NULL
      7      am       vs fisherte…          control        list    NULL
      8      am       vs fisherte…               or           1       1
      9      am       vs fisherte…         conf.int        TRUE    NULL
      10     am       vs fisherte…       conf.level        0.95       1
      11     am       vs fisherte… simulate.p.value       FALSE    NULL
      12     am       vs fisherte…                B        2000       1
    Message
      i 2 more variables: warning, error

# tidy_as_ard() works / when primary stats function errors

    Code
      ard
    Message
      {cards} data frame: 15 x 8
    Output
         group1 variable   context        stat_name        stat     error
      1      am       vs fisherte…         estimate             Planned …
      2      am       vs fisherte…          p.value             Planned …
      3      am       vs fisherte…         conf.low             Planned …
      4      am       vs fisherte…        conf.high             Planned …
      5      am       vs fisherte…           method             Planned …
      6      am       vs fisherte…      alternative             Planned …
      7      am       vs fisherte…        workspace       2e+05 Planned …
      8      am       vs fisherte…           hybrid       FALSE Planned …
      9      am       vs fisherte…       hybridPars c, 5, 80, 1 Planned …
      10     am       vs fisherte…          control        list Planned …
      11     am       vs fisherte…               or           1 Planned …
      12     am       vs fisherte…         conf.int        TRUE Planned …
      13     am       vs fisherte…       conf.level        0.95 Planned …
      14     am       vs fisherte… simulate.p.value       FALSE Planned …
      15     am       vs fisherte…                B        2000 Planned …
    Message
      i 2 more variables: fmt_fun, warning

# tidy_as_ard() works / when `fun_args_to_record` argument is not passed

    Code
      ard
    Message
      {cards} data frame: 3 x 8
    Output
        group1 variable   context stat_name      stat fmt_fun
      1     am       vs fisherte…  estimate     1.956       1
      2     am       vs fisherte…   p.value     0.473       1
      3     am       vs fisherte…    method Fisher's…    NULL
    Message
      i 2 more variables: warning, error

# tidy_as_ard() works / when `formals` argument is not passed.

    Code
      ard
    Message
      {cards} data frame: 3 x 8
    Output
        group1 variable   context stat_name      stat fmt_fun
      1     am       vs fisherte…  estimate     1.956       1
      2     am       vs fisherte…   p.value     0.473       1
      3     am       vs fisherte…    method Fisher's…    NULL
    Message
      i 2 more variables: warning, error

