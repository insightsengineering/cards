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
      i In argument: `stat_fmt = pmap(...)`.
      Caused by error in `.check_fmt_string()`:
      ! unused argument (call = call)

---

    Code
      apply_fmt_fn(dplyr::mutate(ard_fmt_checks, fmt_fn = list(1L, -1L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = pmap(...)`.
      Caused by error in `apply_fmt_fn()`:
      ! The value -1 supplied for `fmt_fn` cannot be applied to the statistic "sd" for the variable "mpg". Formatting functions/aliases must be a function, a non-negative integer, or a formatting string, e.g. "xx.x".

