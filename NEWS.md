# cards (development version)

- Added `ard_categorical(statistic)` argument.
- Added function `categorical_variable_summary_fns()` and made it the default value for `ard_categorical(statistic)`.
- The variable-level statistics regarding missingness are no longer returned by default in `ard_continuous()` and `ard_categorical()`.
- Added function `ard_missing()` to tabulate statistics related to missingness. The default statistics returned are from the new function `missing_variable_summary_fns()`: 'N_obs', 'N_miss', 'N_nonmiss', 'p_miss', 'p_nonmiss'.

