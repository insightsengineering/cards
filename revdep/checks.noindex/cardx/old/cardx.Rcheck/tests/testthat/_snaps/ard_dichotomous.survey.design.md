# ard_dichotomous.survey.design() returns an error with erroneous input

    Code
      ard_dichotomous(svy_dicho, by = vs, variables = c(cyl, am), value = list(cyl = 4),
      denominator = "row")
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `value` for variable "cyl".
      i A value of 4 was passed, but must be one of TRUE and FALSE.
      i To summarize this value, make the column a factor and include 4 as a level.

---

    Code
      ard_dichotomous(svy_dicho, by = cyl, variables = c(vs, am), value = list(vs = 4),
      denominator = "row")
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `value` for variable "vs".
      i A value of 4 was passed, but must be one of 0 and 1.
      i To summarize this value, use `forcats::fct_expand()` to add 4 as a level.

---

    Code
      ard_dichotomous(svy_dicho, by = cyl, variables = c(vs, disp), value = list(
        disp = "turn"), denominator = "row")
    Condition
      Error in `ard_dichotomous()`:
      ! Error in argument `value` for variable "disp".
      i A value of "turn" was passed, but must be one of 71.1, 75.7, 78.7, 79, 95.1, 108, 120.1, 120.3, 121, 140.8, 145, 146.7, 160, 167.6, 225, 258, 275.8, 301, ..., 460, and 472.
      i To summarize this value, make the column a factor and include "turn" as a level.

