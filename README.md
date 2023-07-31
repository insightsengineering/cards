
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

ard_continuous(mtcars, by = cyl, include = c(mpg, hp)) |> 
  flatten_ard()
#> # A tibble: 42 × 6
#>    strata1 strata1_level variable stat_name statistic        context   
#>    <chr>           <dbl> <chr>    <chr>     <chr>            <chr>     
#>  1 cyl                 4 mpg      N         11               continuous
#>  2 cyl                 4 mpg      N_miss    0                continuous
#>  3 cyl                 4 mpg      N_tot     11               continuous
#>  4 cyl                 4 mpg      mean      26.6636363636364 continuous
#>  5 cyl                 4 mpg      sd        4.50982765242148 continuous
#>  6 cyl                 4 mpg      min       21.4             continuous
#>  7 cyl                 4 mpg      max       33.9             continuous
#>  8 cyl                 4 hp       N         11               continuous
#>  9 cyl                 4 hp       N_miss    0                continuous
#> 10 cyl                 4 hp       N_tot     11               continuous
#> # ℹ 32 more rows

ard_categorical(mtcars, by = cyl, include = c(am, gear)) |> 
  flatten_ard()
#> # A tibble: 48 × 7
#>    strata1 strata1_level variable variable_level context     stat_name statistic
#>    <chr>           <dbl> <chr>    <chr>          <chr>       <chr>     <chr>    
#>  1 cyl                 4 am       0              categorical n         3        
#>  2 cyl                 4 am       0              categorical p         0.272727…
#>  3 cyl                 4 am       1              categorical n         8        
#>  4 cyl                 4 am       1              categorical p         0.727272…
#>  5 cyl                 6 am       0              categorical n         4        
#>  6 cyl                 6 am       0              categorical p         0.571428…
#>  7 cyl                 6 am       1              categorical n         3        
#>  8 cyl                 6 am       1              categorical p         0.428571…
#>  9 cyl                 8 am       0              categorical n         12       
#> 10 cyl                 8 am       0              categorical p         0.857142…
#> # ℹ 38 more rows

ard_ttest(data = mtcars, by = "am", variable = "hp") |> 
  flatten_ard()
#> # A tibble: 10 × 6
#>    strata1 variable context stat_name   statistic               strata1_level
#>    <chr>   <chr>    <chr>   <chr>       <chr>                   <chr>        
#>  1 am      hp       ttest   estimate    33.417004048583         <NA>         
#>  2 am      hp       ttest   estimate1   160.263157894737        0            
#>  3 am      hp       ttest   estimate2   126.846153846154        1            
#>  4 am      hp       ttest   statistic   1.26618876980934        <NA>         
#>  5 am      hp       ttest   p.value     0.220979581335913       <NA>         
#>  6 am      hp       ttest   parameter   18.7154096625045        <NA>         
#>  7 am      hp       ttest   conf.low    -21.8785802016468       <NA>         
#>  8 am      hp       ttest   conf.high   88.7125882988128        <NA>         
#>  9 am      hp       ttest   method      Welch Two Sample t-test <NA>         
#> 10 am      hp       ttest   alternative two.sided               <NA>

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
    ard_continuous(mtcars, by = cyl, include = mpg),
    ard_categorical(mtcars, by = cyl, include = am),
    ard_categorical(mtcars, include = cyl)
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
