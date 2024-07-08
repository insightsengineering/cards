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

---

    Code
      ard_stack(ADSL, by = "ARM", ard_continuous(variables = AGE))
    Condition
      Error in `ard_stack()`:
      ! Cannot evaluate expression `by = ARM`.
      i Did you mean `.by = ARM`?

# ard_stack() complex call error

    Code
      complex_call <- list()
      complex_call$ard_continuous <- ard_continuous
      ard_stack(data = mtcars, .by = am, complex_call$ard_continuous(variables = "mpg"),
      )
    Condition
      Error in `ard_stack()`:
      ! `cards::ard_stack()` works with simple calls (`?rlang::call_name()`) and `complex_call$ard_continuous(variables = "mpg")` is not simple.

