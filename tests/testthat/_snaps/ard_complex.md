# ard_complex() messaging

    Code
      ard_complex(ADSL, by = "ARM", variables = c("AGE", "BMIBL"), statistic = list(
        AGE = list(mean = function(x, ...) mean(x))))
    Condition
      Error in `ard_complex()`:
      ! The following columns do not have `statistic` defined: "BMIBL".

# ard_complex() errors with incorrect factor columns

    Code
      ard_complex(dplyr::mutate(mtcars, am = factor(am, levels = character(0))), by = "am",
      variables = "mpg", statistic = list(mpg = list(mean = function(x, ...) mean(x))))
    Condition
      Error in `ard_complex()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_complex(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1, NA),
      exclude = NULL)), by = "am", variables = "mpg", statistic = list(mpg = list(
        mean = function(x, ...) mean(x))))
    Condition
      Error in `ard_complex()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

