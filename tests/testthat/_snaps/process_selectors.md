# process_formula_selectors() error messaging

    Code
      process_formula_selectors(mtcars, variables = list(letters))
    Condition
      Error:
      ! The `variables` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

# compute_formula_selector() selects the last assignment when multiple appear

    Code
      lapply(lst_compute_test, function(x) structure(x, .Environment = NULL))
    Output
      $hp
      [1] "THE DEFAULT"
      
      $mpg
      [1] "Special for MPG"
      

