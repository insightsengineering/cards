# Deprecated functions

**\[deprecated\]**  
Some functions have been deprecated and are no longer being actively
supported.

**Renamed functions**

- `ard_categorical()` to
  [`ard_tabulate()`](https://insightsengineering.github.io/cards/reference/ard_tabulate.md)

- `ard_continuous()` to
  [`ard_summary()`](https://insightsengineering.github.io/cards/reference/ard_summary.md)

- `ard_complex()` to
  [`ard_mvsummary()`](https://insightsengineering.github.io/cards/reference/ard_mvsummary.md)

- `apply_fmt_fn()` to
  [`apply_fmt_fun()`](https://insightsengineering.github.io/cards/reference/apply_fmt_fun.md)

- `alias_as_fmt_fn()` to
  [`alias_as_fmt_fun()`](https://insightsengineering.github.io/cards/reference/alias_as_fmt_fun.md)

- `update_ard_fmt_fn()` to
  [`update_ard_fmt_fun()`](https://insightsengineering.github.io/cards/reference/update_ard.md)

## Usage

``` r
ard_continuous(data, ...)

ard_categorical(data, ...)

ard_complex(data, ...)

ard_dichotomous(data, ...)

# S3 method for class 'data.frame'
ard_continuous(data, ...)

# S3 method for class 'data.frame'
ard_categorical(data, ...)

# S3 method for class 'data.frame'
ard_complex(data, ...)

# S3 method for class 'data.frame'
ard_dichotomous(data, ...)

apply_fmt_fn(...)

alias_as_fmt_fn(...)

update_ard_fmt_fn(...)
```

## Arguments

- data, ...:

  **\[deprecated\]**
