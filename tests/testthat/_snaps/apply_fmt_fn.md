# apply_fmt_fn() error messaging

    Code
      apply_fmt_fn(letters)
    Condition
      Error in `apply_fmt_fn()`:
      ! The `x` argument must be class <card>, not a character vector.

---

    Code
      apply_fmt_fn(dplyr::mutate(ard_fmt_checks, fmt_fn = list("xoxo", "xoxo")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = pmap(...)`.
      Caused by error in `apply_fmt_fn()`:
      ! There was an error applying the formatting function to statistic "mean" for variable "mpg".
      i Perhaps try formmatting function `as.character()`? See error message below:
      x The format "xoxo" for `fmt_fn` is not valid for the variable "mpg" for the statistic "mean". String must begin with 'x' and only consist of x's, a single period or none, and may end with a percent symbol.

---

    Code
      apply_fmt_fn(dplyr::mutate(ard_fmt_checks, fmt_fn = list(1L, -1L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = pmap(...)`.
      Caused by error in `apply_fmt_fn()`:
      ! There was an error applying the formatting function to statistic "sd" for variable "mpg".
      i Perhaps try formmatting function `as.character()`? See error message below:
      x The value -1 supplied for `fmt_fn` cannot be applied to the statistic "sd" for the variable "mpg". Formatting functions/aliases must be a function, a non-negative integer, or a formatting string, e.g. "xx.x".

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

