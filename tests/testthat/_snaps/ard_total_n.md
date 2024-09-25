# ard_total_n() works

    Code
      as.data.frame(ard_total_n(ADSL))
    Output
               variable context stat_name stat_label stat fmt_fn warning error
      1 ..ard_total_n.. total_n         N          N  254      0    NULL  NULL

---

    Code
      ard_total_n(letters)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'ard_total_n' applied to an object of class "character"

# ard_total_n errors with incomplete factor columns

    Code
      ard_total_n(dplyr::mutate(mtcars, am = factor(am)))
    Message
      {cards} data frame: 1 x 8
    Output
               variable context stat_name stat_label stat fmt_fn
      1 ..ard_total_n.. total_n         N          N   32      0
    Message
      i 2 more variables: warning, error

---

    Code
      ard_total_n(dplyr::mutate(mtcars, am = factor(am, levels = character(0))))
    Condition
      Error in `ard_total_n()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_total_n(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1, NA),
      exclude = NULL)))
    Condition
      Error in `ard_total_n()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

