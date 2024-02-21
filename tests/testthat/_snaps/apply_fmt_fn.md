# apply_fmt_fn() error messaging

    Code
      apply_fmt_fn(letters)
    Condition
      Error in `apply_fmt_fn()`:
      i Argument `x` must be class <card>.

---

    Code
      apply_fmt_fn(dplyr::mutate(ard_fmt_checks, fmt_fn = list("xoxo", "xoxo")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = map2(...)`.
      Caused by error:
      ! The format "xoxo" is not valid.

---

    Code
      apply_fmt_fn(dplyr::mutate(ard_fmt_checks, fmt_fn = list(-1L, -1L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = map2(...)`.
      Caused by error:
      ! Formatting functions/aliases must be a function, a non-negative integer, or a formatting string, e.g. "xx.x".

---

    Code
      as.data.frame(apply_fmt_fn(dplyr::mutate(ard_fmt_checks, stat = lapply(stat,
        function(x) x * 1000), fmt_fn = list("xx", "xx"))))
    Output
        variable    context stat_name stat_label     stat stat_fmt fmt_fn warning
      1      mpg continuous      mean       Mean 20090.62    20091     xx    NULL
      2      mpg continuous        sd         SD 6026.948     6027     xx    NULL
        error
      1  NULL
      2  NULL

