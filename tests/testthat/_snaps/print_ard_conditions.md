# print_ard_conditions() works

    Code
      print_ard_conditions(ard_continuous(ADSL, variables = AGE))

---

    Code
      print_ard_conditions(ard_continuous(ADSL, variables = AGE, statistic = ~ list(
        mean = function(x) mean(x), mean_warning = function(x) {
          warning("warn1")
          warning("warn2")
          mean(x)
        }, err_fn = function(x) stop("'tis an error"))))
    Message
      The following errors were returned during `print_ard_conditions()`:
      x For variable `AGE` and "err_fn" statistic: 'tis an error
      The following warnings were returned during `print_ard_conditions()`:
      ! For variable `AGE` and "mean_warning" statistic: warn1
      ! For variable `AGE` and "mean_warning" statistic: warn2

---

    Code
      print_ard_conditions(ard_continuous(ADSL, variables = AGE, by = ARM, statistic = ~
         list(mean = function(x) mean(x), mean_warning = function(x) {
          warning("warn1")
          warning("warn2")
          mean(x)
        }, err_fn = function(x) stop("'tis an error"))))
    Message
      The following errors were returned during `print_ard_conditions()`:
      x For variable `AGE` (`ARM = "Placebo"`) and "err_fn" statistic: 'tis an error
      x For variable `AGE` (`ARM = "Xanomeline High Dose"`) and "err_fn" statistic: 'tis an error
      x For variable `AGE` (`ARM = "Xanomeline Low Dose"`) and "err_fn" statistic: 'tis an error
      The following warnings were returned during `print_ard_conditions()`:
      ! For variable `AGE` (`ARM = "Placebo"`) and "mean_warning" statistic: warn1
      ! For variable `AGE` (`ARM = "Placebo"`) and "mean_warning" statistic: warn2
      ! For variable `AGE` (`ARM = "Xanomeline High Dose"`) and "mean_warning" statistic: warn1
      ! For variable `AGE` (`ARM = "Xanomeline High Dose"`) and "mean_warning" statistic: warn2
      ! For variable `AGE` (`ARM = "Xanomeline Low Dose"`) and "mean_warning" statistic: warn1
      ! For variable `AGE` (`ARM = "Xanomeline Low Dose"`) and "mean_warning" statistic: warn2

---

    Code
      print_ard_conditions(dplyr::mutate(ard_continuous(ADSL, variables = AGE),
      error = list("repeated error")))
    Message
      The following errors were returned during `print_ard_conditions()`:
      x For variable `AGE` and "N", "mean", "sd", "median", "p25", "p75", "min", and "max" statistics: repeated error

---

    Code
      tbl_summary <- (function() {
        set_cli_abort_call()
        ard <- ard_continuous(ADSL, variables = AGE, statistic = ~ list(err_fn = function(
          x) stop("'tis an error")))
        print_ard_conditions(ard)
      })
      tbl_summary()
    Message
      The following errors were returned during `tbl_summary()`:
      x For variable `AGE` and "err_fn" statistic: 'tis an error

# print_ard_conditions() no error when 'error'/'warning' columns not present

    Code
      print_ard_conditions(dplyr::select(ard_continuous(ADSL, variables = AGE),
      -warning, -error))

# print_ard_conditions() no error when factors are present

    Code
      print_ard_conditions(ard)
    Message
      The following warnings were returned during `print_ard_conditions()`:
      ! For variable `continuous_var` (`by_var = "cohort_1"`) and "min" statistic: no non-missing arguments to min; returning Inf
      ! For variable `continuous_var` (`by_var = "cohort_1"`) and "max" statistic: no non-missing arguments to max; returning -Inf

# print_ard_conditions() works when curly brackets appear in condition message

    Code
      print_ard_conditions(ard)
    Message
      The following errors were returned during `print_ard_conditions()`:
      x For variable `AGE` and "mean" statistic: error with {curly} brackets
      The following warnings were returned during `print_ard_conditions()`:
      ! For variable `AGE` and "mean" statistic: warning with {curly} brackets

