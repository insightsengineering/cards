# rename_ard_columns(unlist)

    Code
      dplyr::select(as.data.frame(rename_ard_columns(apply_fmt_fn(ard_categorical(ADSL, by = ARM, variables = AGEGR1)),
      unlist = c(stat, stat_fmt))), -c(fmt_fn, warning, error))
    Output
                          ARM AGEGR1     context stat_name stat_label       stat stat_fmt
      1               Placebo  65-80 categorical         n          n 42.0000000       42
      2               Placebo  65-80 categorical         N          N 86.0000000       86
      3               Placebo  65-80 categorical         p          %  0.4883721     48.8
      4               Placebo    <65 categorical         n          n 14.0000000       14
      5               Placebo    <65 categorical         N          N 86.0000000       86
      6               Placebo    <65 categorical         p          %  0.1627907     16.3
      7               Placebo    >80 categorical         n          n 30.0000000       30
      8               Placebo    >80 categorical         N          N 86.0000000       86
      9               Placebo    >80 categorical         p          %  0.3488372     34.9
      10 Xanomeline High Dose  65-80 categorical         n          n 55.0000000       55
      11 Xanomeline High Dose  65-80 categorical         N          N 84.0000000       84
      12 Xanomeline High Dose  65-80 categorical         p          %  0.6547619     65.5
      13 Xanomeline High Dose    <65 categorical         n          n 11.0000000       11
      14 Xanomeline High Dose    <65 categorical         N          N 84.0000000       84
      15 Xanomeline High Dose    <65 categorical         p          %  0.1309524     13.1
      16 Xanomeline High Dose    >80 categorical         n          n 18.0000000       18
      17 Xanomeline High Dose    >80 categorical         N          N 84.0000000       84
      18 Xanomeline High Dose    >80 categorical         p          %  0.2142857     21.4
      19  Xanomeline Low Dose  65-80 categorical         n          n 47.0000000       47
      20  Xanomeline Low Dose  65-80 categorical         N          N 84.0000000       84
      21  Xanomeline Low Dose  65-80 categorical         p          %  0.5595238     56.0
      22  Xanomeline Low Dose    <65 categorical         n          n  8.0000000        8
      23  Xanomeline Low Dose    <65 categorical         N          N 84.0000000       84
      24  Xanomeline Low Dose    <65 categorical         p          %  0.0952381      9.5
      25  Xanomeline Low Dose    >80 categorical         n          n 29.0000000       29
      26  Xanomeline Low Dose    >80 categorical         N          N 84.0000000       84
      27  Xanomeline Low Dose    >80 categorical         p          %  0.3452381     34.5

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

