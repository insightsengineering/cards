# process_selectors() works

    Code
      process_selectors(mtcars, variables = not_a_column)
    Condition
      Error:
      ! There was an error selecting the `variables` argument. See message below:
      i Can't subset columns that don't exist. x Column `not_a_column` doesn't exist.

# process_formula_selectors() error messaging

    Code
      process_formula_selectors(mtcars, variables = list(letters))
    Condition
      Error:
      ! The `variables` argument must be a named list, list of formulas, or a single formula.

# compute_formula_selector() selects the last assignment when multiple appear

    Code
      compute_formula_selector(data = mtcars[c("mpg", "hp")], x = list(everything() ~
        "THE DEFAULT", mpg = "Special for MPG"))
    Output
      $hp
      [1] "THE DEFAULT"
      
      $mpg
      [1] "Special for MPG"
      

