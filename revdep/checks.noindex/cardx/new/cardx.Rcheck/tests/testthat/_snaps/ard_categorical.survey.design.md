# ard_categorical.survey.design() returns an error when variables have all NAs

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "row")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "column")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

---

    Code
      ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived,
      denominator = "cell")
    Condition
      Error in `ard_categorical()`:
      ! Column "Class" is all missing and cannot be tabulated.
      i Only columns of class <logical> and <factor> can be tabulated when all values are missing.

# ard_categorical.survey.design(by) messages about protected names

    Code
      ard_categorical(svy_mtcars, by = variable, variables = gear)
    Condition
      Error in `ard_categorical()`:
      ! The `by` argument cannot include variables named "variable", "variable_level", "group1_level", "p", and "n".

---

    Code
      ard_categorical(svy_mtcars, by = p.std.error, variables = name)
    Condition
      Error in `ard_categorical()`:
      ! The `variables` argument cannot include variables named "by", "name", "n", "p", and "p.std.error".

