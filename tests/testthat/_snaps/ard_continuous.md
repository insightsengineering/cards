# ard_continuous() messaging

    Code
      ard_continuous(mtcars, variables = "mpg", statistics = ~ list(mean = "this is a string"))
    Condition
      Error:
      ! Error in the argument `statistics` for variable "mpg".
      i Value must be a named list of functions.

