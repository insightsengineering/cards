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
      apply_statistic_fmt_fn(dplyr::mutate(ard_fmt_checks, statistic = lapply(
        statistic, function(x) x * 1000), statistic_fmt_fn = list("xx", "xx")))
    Condition
      Warning:
      There were 2 warnings in `dplyr::mutate()`.
      The first warning was:
      i In argument: `statistic_fmt = map2(...)`.
      Caused by warning:
      ! Formatted statistic, "20091", is longer than allowed by format "xx"
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
      # A tibble: 2 x 9
        variable context stat_name stat_label statistic statistic_fmt statistic_fmt_fn
        <chr>    <chr>   <chr>     <chr>      <list>    <list>        <list>          
      1 mpg      contin~ mean      Mean       <dbl [1]> <chr [1]>     <chr [1]>       
      2 mpg      contin~ sd        SD         <dbl [1]> <chr [1]>     <chr [1]>       
      # i 2 more variables: warning <list>, error <list>

