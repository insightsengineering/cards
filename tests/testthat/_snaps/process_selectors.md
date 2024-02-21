# process_selectors() works

    Code
      process_selectors(mtcars, variables = not_a_column)
    Condition
      Error:
      ! Error processing `variables` argument.
      ! Can't subset columns that don't exist. x Column `not_a_column` doesn't exist.
      i Select among columns "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", and "carb"

# process_formula_selectors() error messaging

    Code
      process_formula_selectors(mtcars, variables = list(letters))
    Condition
      Error:
      ! The `variables` argument must be a named list, list of formulas, a single formula, or empty.
      i Review ?syntax (`?cards::syntax()`) for examples and details.

---

    Code
      process_formula_selectors(mtcars, variables = list(not_a_column ~ letters))
    Condition
      Error:
      ! Error processing `variables` argument.
      ! Can't subset columns that don't exist. x Column `not_a_column` doesn't exist.
      i Select among columns "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", and "carb"

# compute_formula_selector() selects the last assignment when multiple appear

    Code
      lapply(lst_compute_test, function(x) structure(x, .Environment = NULL))
    Output
      $hp
      [1] "THE DEFAULT"
      
      $mpg
      [1] "Special for MPG"
      

