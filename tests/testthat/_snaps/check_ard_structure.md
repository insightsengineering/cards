# check_ard_structure() works

    Code
      check_ard_structure(structure(dplyr::select(dplyr::mutate(ard_summary(ADSL,
        variables = "AGE"), stat = unlist(stat)), -error), class = "data.frame"))
    Message
      Object is not of class <card>.
      The following columns are not present: "error".
      Expecting a row with `stat_name = 'method'`, but it is not present.
      The following columns are expected to be list columns: "stat".

# check_ard_structure() does not error if the tested dataset has none of the expected variables

    Code
      expect_no_error(check_ard_structure(data.frame(badname = 3)))
    Message
      Object is not of class <card>.
      The following columns are not present: "variable", "stat_name", "stat_label", "stat", "fmt_fun", "warning", and "error".
      Expecting a row with `stat_name = 'method'`, but it is not present.

# check_ard_structure() errors when flagged appropriately

    Code
      check_ard_structure(data.frame(badname = 3), error_on_fail = TRUE)
    Condition
      Error in `check_ard_structure()`:
      ! Object is not of class <card>.

---

    Code
      check_ard_structure(nolist, error_on_fail = TRUE)
    Condition
      Error in `check_ard_structure()`:
      ! The following columns are expected to be list columns: "stat".

---

    Code
      check_ard_structure(novariable, error_on_fail = TRUE)
    Condition
      Error in `check_ard_structure()`:
      ! The following columns are not present: "variable".

---

    Code
      check_ard_structure(wrongorder, error_on_fail = TRUE)
    Condition
      Error in `check_ard_structure()`:
      ! The column order is not in the standard order.
      i Use `cards::tidy_ard_column_order()` for standard ordering.

