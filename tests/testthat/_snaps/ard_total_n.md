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
      Error in `ard_total_n()`:
      ! The `data` argument must be class <data.frame>, not a character vector.

