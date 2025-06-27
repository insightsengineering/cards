# apply_fmt_fun() error messaging

    Code
      apply_fmt_fun(letters)
    Condition
      Error in `apply_fmt_fun()`:
      ! The `x` argument must be class <card>, not a character vector.

---

    Code
      apply_fmt_fun(dplyr::mutate(ard_fmt_checks, fmt_fun = list("xoxo", "xoxo")))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = pmap(...)`.
      Caused by error in `apply_fmt_fun()`:
      ! There was an error applying the formatting function to statistic "mean" for variable "mpg".
      i Perhaps try formmatting function `as.character()`? See error message below:
      x The format "xoxo" for `fmt_fun` is not valid for the variable "mpg" for the statistic "mean". String must begin with 'x' and only consist of x's, a single period or none, and may end with a percent symbol.

---

    Code
      apply_fmt_fun(dplyr::mutate(ard_fmt_checks, fmt_fun = list(1L, -1L)))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `stat_fmt = pmap(...)`.
      Caused by error in `apply_fmt_fun()`:
      ! There was an error applying the formatting function to statistic "sd" for variable "mpg".
      i Perhaps try formmatting function `as.character()`? See error message below:
      x The value in `fmt_fun` cannot be converted into a function for statistic "sd" and variable "mpg". i Value must be a function, a non-negative integer, or a formatting string, e.g. "xx.x". * See `?cards::alias_as_fmt_fun()` for details.

---

    Code
      as.data.frame(apply_fmt_fun(dplyr::mutate(ard_fmt_checks, stat = lapply(stat,
        function(x) x * 1000), fmt_fun = list("xx", "xx"))))
    Output
        variable    context stat_name stat_label     stat stat_fmt fmt_fun warning
      1      mpg continuous      mean       Mean 20090.62    20091      xx    NULL
      2      mpg continuous        sd         SD 6026.948     6027      xx    NULL
        error
      1  NULL
      2  NULL

# apply_fmt_fun(replace)

    Code
      apply_fmt_fun(ard, replace = FALSE)
    Message
      {cards} data frame: 3 x 10
    Output
        variable variable_level stat_name stat_label stat   stat_fmt
      1   AGEGR1          65-80         n          n  144 144.000000
      2   AGEGR1            <65         n          n   33         33
      3   AGEGR1            >80         n          n   77         77
    Message
      i 4 more variables: context, fmt_fun, warning, error

---

    Code
      apply_fmt_fun(ard, replace = TRUE)
    Message
      {cards} data frame: 3 x 10
    Output
        variable variable_level stat_name stat_label stat stat_fmt
      1   AGEGR1          65-80         n          n  144      144
      2   AGEGR1            <65         n          n   33       33
      3   AGEGR1            >80         n          n   77       77
    Message
      i 4 more variables: context, fmt_fun, warning, error

