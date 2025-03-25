# rename_ard_columns(columns) messsaging

    Code
      rename_ard_columns(ard_categorical(ADSL, by = ARM, variables = AGEGR1),
      columns = all_ard_groups())
    Condition
      Error in `rename_ard_columns()`:
      ! The `column` argument may only select columns using `all_ard_groups("names")` and `all_ard_variables("names")`
      i  Column "group1_level" is not a valid selection.

---

    Code
      rename_ard_columns(ard_categorical(dplyr::rename(ADSL, stat = AGEGR1), by = ARM,
      variables = stat))
    Condition
      Error in `rename_ard_columns()`:
      ! New column name(s) "stat" cannot be added, because they are already present.

