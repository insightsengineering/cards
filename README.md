
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinal

<!-- badges: start -->
<!-- badges: end -->

The goal of cardinal is to …

## Installation

You can install the development version of cardinal from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("insightsengineering/cardinal")
```

## Example

ARD Examples

``` r
library(cardinal)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

ard_continuous(mtcars, by = cyl, variables = c("mpg", "hp")) |> 
  flatten_ard()
#> # A tibble: 42 × 8
#>    strata1 strata1_level variable stat_name statistic      warning error context
#>    <chr>           <dbl> <chr>    <chr>     <chr>          <chr>   <chr> <chr>  
#>  1 cyl                 4 mpg      N         11             <NA>    <NA>  contin…
#>  2 cyl                 4 mpg      N_miss    0              <NA>    <NA>  contin…
#>  3 cyl                 4 mpg      N_tot     11             <NA>    <NA>  contin…
#>  4 cyl                 4 mpg      mean      26.6636363636… <NA>    <NA>  contin…
#>  5 cyl                 4 mpg      sd        4.50982765242… <NA>    <NA>  contin…
#>  6 cyl                 4 mpg      min       21.4           <NA>    <NA>  contin…
#>  7 cyl                 4 mpg      max       33.9           <NA>    <NA>  contin…
#>  8 cyl                 4 hp       N         11             <NA>    <NA>  contin…
#>  9 cyl                 4 hp       N_miss    0              <NA>    <NA>  contin…
#> 10 cyl                 4 hp       N_tot     11             <NA>    <NA>  contin…
#> # ℹ 32 more rows

ard_categorical(mtcars, by = cyl, variables = c("am", "gear")) |> 
  flatten_ard()
#> # A tibble: 48 × 9
#>    strata1 strata1_level variable variable_level warning error context stat_name
#>    <chr>           <dbl> <chr>    <chr>          <chr>   <chr> <chr>   <chr>    
#>  1 cyl                 4 am       0              <NA>    <NA>  catego… n        
#>  2 cyl                 4 am       0              <NA>    <NA>  catego… p        
#>  3 cyl                 4 am       1              <NA>    <NA>  catego… n        
#>  4 cyl                 4 am       1              <NA>    <NA>  catego… p        
#>  5 cyl                 6 am       0              <NA>    <NA>  catego… n        
#>  6 cyl                 6 am       0              <NA>    <NA>  catego… p        
#>  7 cyl                 6 am       1              <NA>    <NA>  catego… n        
#>  8 cyl                 6 am       1              <NA>    <NA>  catego… p        
#>  9 cyl                 8 am       0              <NA>    <NA>  catego… n        
#> 10 cyl                 8 am       0              <NA>    <NA>  catego… p        
#> # ℹ 38 more rows
#> # ℹ 1 more variable: statistic <chr>

ard_ttest(data = mtcars, by = "am", variable = "hp") |> 
  flatten_ard()
#> # A tibble: 10 × 8
#>    strata1 variable stat_name   statistic    strata1_level context warning error
#>    <chr>   <chr>    <chr>       <chr>        <chr>         <chr>   <chr>   <chr>
#>  1 am      hp       estimate    33.41700404… <NA>          t.test  <NA>    <NA> 
#>  2 am      hp       estimate1   160.2631578… 0             t.test  <NA>    <NA> 
#>  3 am      hp       estimate2   126.8461538… 1             t.test  <NA>    <NA> 
#>  4 am      hp       statistic   1.266188769… <NA>          t.test  <NA>    <NA> 
#>  5 am      hp       p.value     0.220979581… <NA>          t.test  <NA>    <NA> 
#>  6 am      hp       parameter   18.71540966… <NA>          t.test  <NA>    <NA> 
#>  7 am      hp       conf.low    -21.8785802… <NA>          t.test  <NA>    <NA> 
#>  8 am      hp       conf.high   88.71258829… <NA>          t.test  <NA>    <NA> 
#>  9 am      hp       method      Welch Two S… <NA>          t.test  <NA>    <NA> 
#> 10 am      hp       alternative two.sided    <NA>          t.test  <NA>    <NA>

glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial) |>
  ard_regression(add_estimate_to_reference_rows = TRUE) |> 
  flatten_ard()
#> # A tibble: 68 × 5
#>    variable variable_level stat_name      statistic  context   
#>    <chr>    <chr>          <chr>          <chr>      <chr>     
#>  1 mpg      <NA>           term           mpg        regression
#>  2 mpg      <NA>           var_label      mpg        regression
#>  3 mpg      <NA>           var_class      numeric    regression
#>  4 mpg      <NA>           var_type       continuous regression
#>  5 mpg      <NA>           var_nlevels    <NA>       regression
#>  6 mpg      <NA>           contrasts      <NA>       regression
#>  7 mpg      <NA>           contrasts_type <NA>       regression
#>  8 mpg      <NA>           reference_row  <NA>       regression
#>  9 mpg      <NA>           label          mpg        regression
#> 10 mpg      <NA>           n_obs          32         regression
#> # ℹ 58 more rows
```

ARD -\> Table Example

``` r
# Construct the ARD
table_ard <-
  bind_rows(
    ard_continuous(mtcars, by = cyl, variables = "mpg"),
    ard_categorical(mtcars, by = cyl, variables = "am"),
    ard_categorical(mtcars, variables = "cyl")
  )

# convert ARD to a cardinal table
table <-
  construct_cardinal(
    table_plan =
      bind_rows(
        table_ard |> filter(variable %in% "mpg") |>  table_plan_simple_continuous(),
        table_ard |> filter(variable %in% "am") |> table_plan_simple_categorical()
      ),
    header_plan =
      table_ard |>
      filter(variable %in% "cyl") |>
      header_plan_simple(header = "**{strata} Cylinders**  \nN={n}  ({p}%)") |>
      modifyList(val = list(label = gt::md("**Characteristic**")))
  ) |>
  convert_cardinal(engine = "gt")
```

<img src="man/figures/README-table_example.png" style="width: 50%">
