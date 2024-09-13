# update_ard_fmt_fn()

    Code
      update_ard_fmt_fn(ard_continuous(ADSL, variables = AGE), stat_names = c("mean",
        "sd"), fmt_fn = -8L)
    Condition
      Error in `update_ard_fmt_fn()`:
      ! The value in `fmt_fn` cannot be converted into a function.
      i Value must be a function, a non-negative integer, or a formatting string, e.g. "xx.x".
      * See `?cards::alias_as_fmt_fn()` for details.

# update_ard_fmt_fn(filter)

    Code
      apply_fmt_fn(update_ard_fmt_fn(ard_continuous(ADSL, by = ARM, variables = AGE,
        statistic = ~ continuous_summary_fns(c("N", "mean"))), stat_names = "mean",
      fmt_fn = 8L, filter = group1_level == "Placebo"))
    Message
      {cards} data frame: 6 x 11
    Output
        group1 group1_level variable stat_name stat_label   stat    stat_fmt
      1    ARM      Placebo      AGE         N          N     86          86
      2    ARM      Placebo      AGE      mean       Mean 75.209 75.20930233
      3    ARM    Xanomeli…      AGE         N          N     84          84
      4    ARM    Xanomeli…      AGE      mean       Mean 74.381        74.4
      5    ARM    Xanomeli…      AGE         N          N     84          84
      6    ARM    Xanomeli…      AGE      mean       Mean 75.667        75.7
    Message
      i 4 more variables: context, fmt_fn, warning, error

