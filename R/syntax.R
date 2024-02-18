#' Selecting Syntax
#'
#' @name syntax
#' @keywords internal
#'
#' @description
#' # Selectors
#'
#' The cards package also utilizes selectors: selectors from the tidyselect
#' package and custom selectors. Review their help files for details.
#'
#' - **tidy selectors**
#'
#'     [everything()], [all_of()], [any_of()], [starts_with()], [ends_with()],
#'     [contains()], [matches()], [num_range()], [last_col()]
#'
#' - **cards selectors**
#'
#'     [all_ard_groups()], [all_ard_variables()]
#'
#' # Formula and List Selectors
#'
#' Some arguments in the cards package accept list and
#' formula notation, e.g. `ard_continuous(statistic=)`.
#' Below enumerates a few tips and shortcuts for using the list and formulas.
#'
#' 1. **List of Formulas**
#'
#'     Typical usage includes a list of formulas, where the LHS is a variable
#'     name or a selector.
#'
#'     ```r
#'     ard_continuous(statistic = list(age ~ list(N = \(x) length(x)), starts_with("a") ~ list(mean = mean)))
#'     ```
#'
#' 2. **Named List**
#'
#'     You may also pass a named list; however, the tidyselect selectors
#'     are not supported with this syntax.
#'
#'     ```r
#'     ard_continuous(statistic = list(age = list(N = \(x) length(x))))
#'     ```
#'
#' 3. **Hybrid Named List/List of Formulas**
#'
#'     You can pass a combination of formulas and named elements.
#'
#'     ```r
#'     ard_continuous(statistic = list(age = list(N = \(x) length(x)), starts_with("a") ~ list(mean = mean)))
#'     ```
#'
#' 4. **Shortcuts**
#'
#'     You can pass a single formula, which is equivalent to passing the formula
#'     in a list.
#'
#'     ```r
#'     ard_continuous(statistic = starts_with("a") ~ list(mean = mean)
#'     ```
#'
#'     As a shortcut to select all variables, you can omit the LHS of the formula.
#'     The two calls below are equivalent.
#'
#'     ```r
#'     ard_continuous(statistic = ~list(N = \(x) length(x)))
#'     ard_continuous(statistic = everything() ~ list(N = \(x) length(x)))
#'     ```
#'
#' 5. **Combination Selectors**
#'
#'     Selectors can be combined using the `c()` function.
#'
#'     ```r
#'     ard_continuous(statistic = c(everything(), -age) ~ list(N = \(x) length(x)))
#'     ```
NULL
