# ard_dichotomous() works

    Code
      class(ard_dich)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

---

    Code
      as.data.frame(dplyr::select(ard_dich, -c(fmt_fun, warning, error)))
    Output
        variable variable_level     context stat_name stat_label    stat
      1      cyl              4 dichotomous         n          n      11
      2      cyl              4 dichotomous         N          N      32
      3      cyl              4 dichotomous         p          % 0.34375
      4       am           TRUE dichotomous         n          n      13
      5       am           TRUE dichotomous         N          N      32
      6       am           TRUE dichotomous         p          % 0.40625
      7     gear              3 dichotomous         n          n       5
      8     gear              3 dichotomous         N          N      32
      9     gear              3 dichotomous         p          % 0.15625

---

    Code
      ard_dichotomous(mtcars, variables = c("cyl", "am", "gear"), value = list(cyl = letters))
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `value` for variable "cyl".
      i The value must be one of 4, 6, and 8.

---

    Code
      ard_dichotomous(iris, variables = everything(), value = list(Species = "not_a_species"))
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `value` for variable "Species".
      i A value of "not_a_species" was passed, but must be one of setosa, versicolor, and virginica.
      i To summarize this value, use `forcats::fct_expand()` to add "not_a_species" as a level.

---

    Code
      ard_dichotomous(mtcars, variables = c("cyl", "am", "gear"), value = list(cyl = 100))
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `value` for variable "cyl".
      i A value of 100 was passed, but must be one of 4, 6, and 8.
      i To summarize this value, make the column a factor and include 100 as a level.

# ard_dichotomous() errors with incomplete factor columns

    Code
      ard_dichotomous(dplyr::mutate(mtcars, am = factor(am, levels = character(0))),
      variables = c(cyl, vs), by = am, value = list(cyl = 4))
    Condition
      Error in `ard_dichotomous()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_dichotomous(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1, NA),
      exclude = NULL)), variables = c(cyl, am), value = list(cyl = 4))
    Condition
      Error in `ard_dichotomous()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

