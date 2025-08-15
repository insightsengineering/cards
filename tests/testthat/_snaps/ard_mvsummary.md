# ard_mvsummary() messaging

    Code
      ard_mvsummary(ADSL, by = "ARM", variables = c("AGE", "BMIBL"), statistic = list(
        AGE = list(mean = function(x, ...) mean(x))))
    Condition
      Error in `ard_mvsummary()`:
      ! The following columns do not have `statistic` defined: "BMIBL".

# ard_mvsummary() errors with incorrect factor columns

    Code
      ard_mvsummary(dplyr::mutate(mtcars, am = factor(am, levels = character(0))),
      by = "am", variables = "mpg", statistic = list(mpg = list(mean = function(x,
        ...) mean(x))))
    Condition
      Error in `ard_mvsummary()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "am".

---

    Code
      ard_mvsummary(dplyr::mutate(mtcars, am = factor(am, levels = c(0, 1, NA),
      exclude = NULL)), by = "am", variables = "mpg", statistic = list(mpg = list(
        mean = function(x, ...) mean(x))))
    Condition
      Error in `ard_mvsummary()`:
      ! Factors with NA levels are not allowed, which are present in column "am".

