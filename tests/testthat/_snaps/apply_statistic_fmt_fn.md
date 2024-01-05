# apply_statistic_fmt_fn() error messaging

    Code
      apply_statistic_fmt_fn(letters)
    Condition
      Error in `apply_statistic_fmt_fn()`:
      i Argument `x` must be class <card>.

---

    Code
      apply_statistic_fmt_fn(dplyr::mutate(ard_fmt_checks, statistic_fmt_fn = list(
        "xoxo", "xoxo")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `statistic_fmt = map2(...)`.
      Caused by error:
      ! The format "xoxo" is not valid.

---

    Code
      apply_statistic_fmt_fn(dplyr::mutate(ard_fmt_checks, statistic_fmt_fn = list(
        -1L, -1L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `statistic_fmt = map2(...)`.
      Caused by error:
      ! Formatting functions/aliases must be a function, a non-negative integer, or a formatting string, e.g. "xx.x".

---

    Code
      as.data.frame(apply_statistic_fmt_fn(dplyr::mutate(ard_fmt_checks, statistic = lapply(
        statistic, function(x) x * 1000), statistic_fmt_fn = list("xx", "xx"))))
    Output
        variable    context stat_name stat_label statistic statistic_fmt
      1      mpg continuous      mean       Mean  20090.62         20091
      2      mpg continuous        sd         SD  6026.948          6027
        statistic_fmt_fn warning error
      1               xx    NULL  NULL
      2               xx    NULL  NULL

