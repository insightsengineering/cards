# ard_dichotomous() works

    Code
      class(ard_dich)
    Output
      [1] "card"       "tbl_df"     "tbl"        "data.frame"

---

    Code
      as.data.frame(dplyr::select(ard_dich, -c(statistic_fmt_fn, warning, error)))
    Output
        variable variable_level     context stat_name stat_label statistic
      1       am           TRUE dichotomous         n          n        13
      2       am           TRUE dichotomous         N          N        32
      3       am           TRUE dichotomous         p          %   0.40625
      4      cyl              4 dichotomous         n          n        11
      5      cyl              4 dichotomous         N          N        32
      6      cyl              4 dichotomous         p          %   0.34375
      7     gear              3 dichotomous         n          n         5
      8     gear              3 dichotomous         N          N        32
      9     gear              3 dichotomous         p          %   0.15625

---

    Code
      ard_dichotomous(mtcars, variables = c("cyl", "am", "gear"), values = list(cyl = letters))
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `values` for variable "cyl".
      i The length of the value must be one and not one of NA, NaN, and Inf.

---

    Code
      ard_dichotomous(iris, variables = everything(), values = list(Species = "not_a_species"))
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `values` for variable "Species".
      i A value of "not_a_species" was passed, but must be one of "setosa", "versicolor", and "virginica".

---

    Code
      ard_dichotomous(mtcars, variables = c("cyl", "am", "gear"), values = list(cyl = 100))
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `values` for variable "cyl".
      i A value of 100 was passed, but must be one of 4, 6, and 8.

