# ard_stack() messaging

    Code
      head(ard_stack(data = mtcars, ard_continuous(variables = "mpg"), .overall = TRUE), 1L)
    Message
      The `.by` argument should be specified when using `.overall=TRUE`.
      i Setting `ard_stack(.overall=FALSE)`.
      {cards} data frame: 1 x 8
    Output
        variable   context stat_name stat_label stat fmt_fun
      1      mpg continuo…         N          N   32       0
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
      ard_stack(data = mtcars, .by = am, complex_call$ard_continuous(variables = "mpg"), )
    Condition
      Error in `ard_stack()`:
      ! `cards::ard_stack()` works with simple calls (`?rlang::call_name()`) and `complex_call$ard_continuous(variables = "mpg")` is not simple.

# ard_stack(.by) messaging

    Code
      dplyr::filter(ard_stack(mtcars2, ard_continuous(variables = "mpg", statistic = ~ continuous_summary_fns("N")), .by = c(am, vs), .total_n = TRUE,
      .overall = TRUE), stat_name %in% "N")
    Message
      * Removing 1 row with NA or NaN values in "am" and "vs" columns.
      {cards} data frame: 10 x 13
    Output
         group1 group1_level group2 group2_level        variable variable_level stat_name stat_label stat
      1      am            0     vs            0             mpg                        N          N   12
      2      am            0     vs            1             mpg                        N          N    7
      3      am            1     vs            0             mpg                        N          N    5
      4      am            1     vs            1             mpg                        N          N    7
      5    <NA>                <NA>                          mpg                        N          N   31
      6    <NA>                <NA>                           am              0         N          N   31
      7    <NA>                <NA>                           am              1         N          N   31
      8    <NA>                <NA>                           vs              0         N          N   31
      9    <NA>                <NA>                           vs              1         N          N   31
      10   <NA>                <NA>              ..ard_total_n..                        N          N   31
    Message
      i 4 more variables: context, fmt_fun, warning, error

---

    Code
      dplyr::filter(ard_stack(mtcars3, ard_continuous(variables = "mpg", statistic = ~ continuous_summary_fns("N")), .by = c(am, vs), .total_n = TRUE,
      .overall = TRUE), stat_name %in% "N")
    Message
      * Removing 2 rows with NA or NaN values in "am" and "vs" columns.
      {cards} data frame: 10 x 13
    Output
         group1 group1_level group2 group2_level        variable variable_level stat_name stat_label stat
      1      am            0     vs            0             mpg                        N          N   12
      2      am            0     vs            1             mpg                        N          N    7
      3      am            1     vs            0             mpg                        N          N    4
      4      am            1     vs            1             mpg                        N          N    7
      5    <NA>                <NA>                          mpg                        N          N   30
      6    <NA>                <NA>                           am              0         N          N   30
      7    <NA>                <NA>                           am              1         N          N   30
      8    <NA>                <NA>                           vs              0         N          N   30
      9    <NA>                <NA>                           vs              1         N          N   30
      10   <NA>                <NA>              ..ard_total_n..                        N          N   30
    Message
      i 4 more variables: context, fmt_fun, warning, error

