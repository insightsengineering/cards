# ard_tabulate_value() works

    Code
      class(ard_dich)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

---

    Code
      as.data.frame(dplyr::select(ard_dich, -c(fmt_fun, warning, error)))
    Output
        variable variable_level        context stat_name stat_label    stat
      1      cyl              4 tabulate_value         n          n      11
      2      cyl              4 tabulate_value         N          N      32
      3      cyl              4 tabulate_value         p          % 0.34375
      4       am           TRUE tabulate_value         n          n      13
      5       am           TRUE tabulate_value         N          N      32
      6       am           TRUE tabulate_value         p          % 0.40625
      7     gear              3 tabulate_value         n          n       5
      8     gear              3 tabulate_value         N          N      32
      9     gear              3 tabulate_value         p          % 0.15625

# ard_tabulate_value() errors are correct

    Code
      ard_tabulate_value(mtcars, variables = c("cyl", "am", "gear"), value = list(
        cyl = letters))
    Condition
      Error in `ard_tabulate_value()`:
      ! Error in argument `value` for variable "cyl".
      i The value must be one of 4, 6, and 8.

---

    Code
      ard_tabulate_value(iris, variables = everything(), value = list(Species = "not_a_species"))
    Condition
      Error in `ard_tabulate_value()`:
      ! Error in argument `value` for variable "Species".
      i A value of "not_a_species" was passed, but must be one of setosa, versicolor, and virginica.
      i To summarize this value, use `forcats::fct_expand()` to add "not_a_species" as a level.

---

    Code
      ard_tabulate_value(mtcars, variables = c("cyl", "am", "gear"), value = list(
        cyl = 100))
    Condition
      Error in `ard_tabulate_value()`:
      ! Error in argument `value` for variable "cyl".
      i A value of 100 was passed, but must be one of 4, 6, and 8.
      i To summarize this value, make the column a factor and include 100 as a level.

# ard_tabulate_value() errors with incomplete factor columns

    Code
      ard_tabulate_value(dplyr::mutate(mtcars, am = factor(am, levels = character(0))),
      variables = c(cyl, vs), by = am, value = list(cyl = 4))
    Condition
      Error in `ard_tabulate_value()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_tabulate_value(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1, NA),
      exclude = NULL)), variables = c(cyl, am), value = list(cyl = 4))
    Condition
      Error in `ard_tabulate_value()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

