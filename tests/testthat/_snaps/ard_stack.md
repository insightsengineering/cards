# ard_stack() messaging

    Code
      head(ard_stack(data = mtcars, ard_continuous(variables = "mpg"), .overall = TRUE),
      1L)
    Message
      The `.by` argument should be specified when using `.overall=TRUE`.
      i Setting `ard_stack(.overall=FALSE)`.
      {cards} data frame: 1 x 8
    Output
        variable   context stat_name stat_label stat fmt_fn
      1      mpg continuo…         N          N   32      0
    Message
      i 2 more variables: warning, error

