# rename_ard_columns(unlist)

    Code
      rename_ard_columns(apply_fmt_fn(ard_categorical(ADSL, by = ARM, variables = AGEGR1)),
      unlist = c(stat, stat_fmt))
    Output
      # A tibble: 27 x 10
         ARM        AGEGR1 context stat_name stat_label   stat stat_fmt fmt_fn warning
         <chr>      <chr>  <chr>   <chr>     <chr>       <dbl> <chr>    <list> <list> 
       1 Placebo    65-80  catego~ n         n          42     42       <int>  <NULL> 
       2 Placebo    65-80  catego~ N         N          86     86       <int>  <NULL> 
       3 Placebo    65-80  catego~ p         %           0.488 48.8     <fn>   <NULL> 
       4 Xanomelin~ 65-80  catego~ n         n          55     55       <int>  <NULL> 
       5 Xanomelin~ 65-80  catego~ N         N          84     84       <int>  <NULL> 
       6 Xanomelin~ 65-80  catego~ p         %           0.655 65.5     <fn>   <NULL> 
       7 Xanomelin~ 65-80  catego~ n         n          47     47       <int>  <NULL> 
       8 Xanomelin~ 65-80  catego~ N         N          84     84       <int>  <NULL> 
       9 Xanomelin~ 65-80  catego~ p         %           0.560 56.0     <fn>   <NULL> 
      10 Placebo    <65    catego~ n         n          14     14       <int>  <NULL> 
      # i 17 more rows
      # i 1 more variable: error <list>

# rename_ard_columns(unlist) messaging

    Code
      head(rename_ard_columns(dplyr::mutate(ard_categorical(ADSL, by = ARM,
        variables = AGEGR1), stat = ifelse(dplyr::row_number() == 1L, list(median),
      stat)), unlist = stat), 1L)
    Message
      Unable to unlist column "stat".
      i This often occurs when a list column contains elements that cannot coerced to a common type.
    Output
      # A tibble: 1 x 9
        ARM     AGEGR1 context     stat_name stat_label stat   fmt_fn warning error 
        <chr>   <chr>  <chr>       <chr>     <chr>      <list> <list> <list>  <list>
      1 Placebo 65-80  categorical n         n          <fn>   <int>  <NULL>  <NULL>

---

    Code
      rename_ard_columns(dplyr::mutate(ard_categorical(ADSL, by = ARM, variables = AGEGR1),
      stat = ifelse(dplyr::row_number() == 1L, list(NULL), stat)), unlist = stat)
    Condition
      Error in `rename_ard_columns()`:
      ! Cannot unlist column "stat". The unlisted result is not the same length as the original.
      i This often occurs when the column contains `NULL` values.
      * Run `cards::replace_null_statistic()` to replace `NULL` values with NA.

