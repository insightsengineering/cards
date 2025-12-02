# Unlist ARD Columns

Unlist ARD Columns

## Usage

``` r
unlist_ard_columns(
  x,
  columns = c(where(is.list), -any_of(c("warning", "error", "fmt_fun"))),
  fill = NA,
  fct_as_chr = TRUE
)
```

## Arguments

- x:

  (`data.frame`)  
  an ARD data frame of class 'card' or any data frame

- columns:

  ([`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))  
  columns to unlist. Default is
  `c(where(is.list), -any_of(c("warning", "error", "fmt_fun")))`.

- fill:

  (scalar)  
  scalar to fill NULL values with before unlisting (if they are
  present). Default is `NA`.

- fct_as_chr:

  (scalar `logical`)  
  When `TRUE`, factor elements will be converted to character before
  unlisting. When the column being unlisted contains mixed types of
  classes, the factor elements are often converted to the underlying
  integer value instead of retaining the label. Default is `TRUE`.

## Value

a data frame

## Examples

``` r
ADSL |>
  ard_tabulate(by = ARM, variables = AGEGR1) |>
  apply_fmt_fun() |>
  unlist_ard_columns()
#> {cards} data frame: 27 x 12
#>    group1 group1_level variable variable_level stat_name stat_label  stat
#> 1     ARM      Placebo   AGEGR1          65-80         n          n    42
#> 2     ARM      Placebo   AGEGR1          65-80         N          N    86
#> 3     ARM      Placebo   AGEGR1          65-80         p          % 0.488
#> 4     ARM      Placebo   AGEGR1            <65         n          n    14
#> 5     ARM      Placebo   AGEGR1            <65         N          N    86
#> 6     ARM      Placebo   AGEGR1            <65         p          % 0.163
#> 7     ARM      Placebo   AGEGR1            >80         n          n    30
#> 8     ARM      Placebo   AGEGR1            >80         N          N    86
#> 9     ARM      Placebo   AGEGR1            >80         p          % 0.349
#> 10    ARM    Xanomeli…   AGEGR1          65-80         n          n    55
#>    stat_fmt
#> 1        42
#> 2        86
#> 3      48.8
#> 4        14
#> 5        86
#> 6      16.3
#> 7        30
#> 8        86
#> 9      34.9
#> 10       55
#> ℹ 17 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, fmt_fun, warning, error

ADSL |>
  ard_summary(by = ARM, variables = AGE) |>
  apply_fmt_fun() |>
  unlist_ard_columns()
#> {cards} data frame: 24 x 11
#>    group1 group1_level variable stat_name stat_label   stat stat_fmt
#> 1     ARM      Placebo      AGE         N          N     86       86
#> 2     ARM      Placebo      AGE      mean       Mean 75.209     75.2
#> 3     ARM      Placebo      AGE        sd         SD   8.59      8.6
#> 4     ARM      Placebo      AGE    median     Median     76     76.0
#> 5     ARM      Placebo      AGE       p25         Q1     69     69.0
#> 6     ARM      Placebo      AGE       p75         Q3     82     82.0
#> 7     ARM      Placebo      AGE       min        Min     52     52.0
#> 8     ARM      Placebo      AGE       max        Max     89     89.0
#> 9     ARM    Xanomeli…      AGE         N          N     84       84
#> 10    ARM    Xanomeli…      AGE      mean       Mean 74.381     74.4
#> ℹ 14 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, fmt_fun, warning, error
```
