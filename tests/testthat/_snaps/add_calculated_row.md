# add_calculated_row(x)

    Code
      apply_fmt_fun(add_calculated_row(ard_continuous(mtcars, variables = mpg), expr = max -
        min, stat_name = "range"))
    Message
      {cards} data frame: 9 x 9
    Output
        variable   context stat_name stat_label   stat stat_fmt
      1      mpg continuo…         N          N     32       32
      2      mpg continuo…      mean       Mean 20.091     20.1
      3      mpg continuo…        sd         SD  6.027      6.0
      4      mpg continuo…    median     Median   19.2     19.2
      5      mpg continuo…       p25         Q1  15.35     15.4
      6      mpg continuo…       p75         Q3   22.8     22.8
      7      mpg continuo…       min        Min   10.4     10.4
      8      mpg continuo…       max        Max   33.9     33.9
      9      mpg continuo…     range      range   23.5     23.5
    Message
      i 3 more variables: fmt_fun, warning, error

---

    Code
      apply_fmt_fun(add_calculated_row(ard_continuous(mtcars, variables = mpg), expr = dplyr::case_when(
        mean > median ~ "Right Skew", mean < median ~ "Left Skew", .default = "Symmetric"),
      stat_name = "skew"))
    Message
      {cards} data frame: 9 x 9
    Output
        variable   context stat_name stat_label      stat   stat_fmt
      1      mpg continuo…         N          N        32         32
      2      mpg continuo…      mean       Mean    20.091       20.1
      3      mpg continuo…        sd         SD     6.027        6.0
      4      mpg continuo…    median     Median      19.2       19.2
      5      mpg continuo…       p25         Q1     15.35       15.4
      6      mpg continuo…       p75         Q3      22.8       22.8
      7      mpg continuo…       min        Min      10.4       10.4
      8      mpg continuo…       max        Max      33.9       33.9
      9      mpg continuo…      skew       skew Right Sk… Right Skew
    Message
      i 3 more variables: fmt_fun, warning, error

# add_calculated_row(expr) messaging

    Code
      add_calculated_row(ard_continuous(mtcars, variables = mpg), expr = not_a_stat *
        2, stat_name = "this_doesnt_work")
    Condition
      Error in `add_calculated_row()`:
      ! There was an error calculating the new statistic. See below:
      x object 'not_a_stat' not found

# add_calculated_row(by) messaging

    Code
      add_calculated_row(ard_continuous(mtcars, variables = mpg, by = cyl), expr = max -
        min, stat_name = "range", by = "context")
    Condition
      Error in `add_calculated_row()`:
      ! Duplicate statistics present within `by` groups: "N", "mean", "sd", "median", "p25", "p75", "min", "max", "N", "mean", "sd", "median", "p25", "p75", "min", and "max"

