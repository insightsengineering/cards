# add_calculated_row(x)

    Code
      out
    Message
      {cards} data frame: 9 x 8
    Output
        variable context stat_name stat_label   stat fmt_fun
      1      mpg summary         N          N     32       0
      2      mpg summary      mean       Mean 20.091       1
      3      mpg summary        sd         SD  6.027       1
      4      mpg summary    median     Median   19.2       1
      5      mpg summary       p25         Q1  15.35       1
      6      mpg summary       p75         Q3   22.8       1
      7      mpg summary       min        Min   10.4       1
      8      mpg summary       max        Max   33.9       1
      9      mpg summary     range      range   23.5       1
    Message
      i 2 more variables: warning, error

---

    Code
      out2
    Message
      {cards} data frame: 9 x 8
    Output
        variable context stat_name stat_label      stat fmt_fun
      1      mpg summary         N          N        32       0
      2      mpg summary      mean       Mean    20.091       1
      3      mpg summary        sd         SD     6.027       1
      4      mpg summary    median     Median      19.2       1
      5      mpg summary       p25         Q1     15.35       1
      6      mpg summary       p75         Q3      22.8       1
      7      mpg summary       min        Min      10.4       1
      8      mpg summary       max        Max      33.9       1
      9      mpg summary      skew       skew Right Sk…    <fn>
    Message
      i 2 more variables: warning, error

# add_calculated_row(expr) errors when a variable is not present

    Code
      add_calculated_row(tbl, expr = not_a_stat * 2, stat_name = "this_doesnt_work")
    Condition
      Error in `add_calculated_row()`:
      ! There was an error calculating the new statistic. See below:
      x object 'not_a_stat' not found

# add_calculated_row(by) messaging

    Code
      add_calculated_row(tbl, expr = max - min, stat_name = "range", by = "context")
    Condition
      Error in `add_calculated_row()`:
      ! Duplicate statistics present within `by` groups: "N", "mean", "sd", "median", "p25", "p75", "min", "max", "N", "mean", "sd", "median", "p25", "p75", "min", and "max"

