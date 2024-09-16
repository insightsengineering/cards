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

# update_ard_fmt_fn(filter) messaging

    Code
      update_ard_fmt_fn(ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~
        continuous_summary_fns(c("N", "mean"))), stat_names = "mean", fmt_fn = 8L,
      filter = group99999999_level == "Placebo")
    Condition
      Error in `update_ard_fmt_fn()`:
      ! There was an error evaluating the `filter` argument. See below:
      x object 'group99999999_level' not found

---

    Code
      update_ard_fmt_fn(ard_continuous(ADSL, by = ARM, variables = AGE, statistic = ~
        continuous_summary_fns(c("N", "mean"))), stat_names = "mean", fmt_fn = 8L,
      filter = c(TRUE, FALSE))
    Condition
      Error in `update_ard_fmt_fn()`:
      ! The `filter` argument must be an expression that evaluates to a <logical> vector of length 1 or 6.

# update_ard_stat_label(filter)

    Code
      update_ard_stat_label(ard_continuous(ADSL, by = ARM, variables = AGE,
        statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))), stat_names = c(
        "mean", "sd"), stat_label = "Mean (SD)", filter = group1_level == "Placebo")
    Message
      {cards} data frame: 9 x 10
    Output
        group1 group1_level variable stat_name stat_label   stat
      1    ARM      Placebo      AGE         N          N     86
      2    ARM      Placebo      AGE      mean  Mean (SD) 75.209
      3    ARM      Placebo      AGE        sd  Mean (SD)   8.59
      4    ARM    Xanomeli…      AGE         N          N     84
      5    ARM    Xanomeli…      AGE      mean       Mean 74.381
      6    ARM    Xanomeli…      AGE        sd         SD  7.886
      7    ARM    Xanomeli…      AGE         N          N     84
      8    ARM    Xanomeli…      AGE      mean       Mean 75.667
      9    ARM    Xanomeli…      AGE        sd         SD  8.286
    Message
      i 4 more variables: context, fmt_fn, warning, error

# update_ard_stat_label(filter) messaging

    Code
      update_ard_stat_label(ard_continuous(ADSL, by = ARM, variables = AGE,
        statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))), stat_names = c(
        "mean", "sd"), stat_label = "Mean (SD)", filter = group99999999_level ==
        "Placebo")
    Condition
      Error in `value[[3L]]()`:
      ! There was an error evaluating the `filter` argument. See below:
      x object 'group99999999_level' not found

---

    Code
      update_ard_stat_label(ard_continuous(ADSL, by = ARM, variables = AGE,
        statistic = ~ continuous_summary_fns(c("N", "mean", "sd"))), stat_names = c(
        "mean", "sd"), stat_label = "Mean (SD)", filter = c(TRUE, FALSE))
    Condition
      Error in `update_ard_stat_label()`:
      ! The `filter` argument must be an expression that evaluates to a <logical> vector of length 1 or 9.

