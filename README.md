
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

This is a basic example which shows you how to solve a common problem:

``` r
library(cardinal)

ard_continuous(mtcars, by = cyl, include = c(mpg, hp)) |> 
  # convert list columns to character for a nicer print
  dplyr::mutate(across(where(is.list), unlist))
#> # A tibble: 42 × 6
#>    strata1 strata1_levels variable stat_name statistic context   
#>    <chr>            <dbl> <chr>    <chr>         <dbl> <chr>     
#>  1 cyl                  6 mpg      N              7    continuous
#>  2 cyl                  6 mpg      N_miss         0    continuous
#>  3 cyl                  6 mpg      N_tot          7    continuous
#>  4 cyl                  6 mpg      mean          19.7  continuous
#>  5 cyl                  6 mpg      sd             1.45 continuous
#>  6 cyl                  6 mpg      min           17.8  continuous
#>  7 cyl                  6 mpg      max           21.4  continuous
#>  8 cyl                  6 hp       N              7    continuous
#>  9 cyl                  6 hp       N_miss         0    continuous
#> 10 cyl                  6 hp       N_tot          7    continuous
#> # ℹ 32 more rows

ard_categorical(mtcars, by = cyl, include = c(am, gear)) |> 
  # convert list columns to character for a nicer print
  dplyr::mutate(across(where(is.list), ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist()))
#> # A tibble: 46 × 7
#>    strata1 strata1_levels variable variable_level context    stat_name statistic
#>    <chr>            <dbl> <chr>             <dbl> <chr>      <chr>         <dbl>
#>  1 cyl                  6 am                    0 categoric… n             4    
#>  2 cyl                  6 am                    0 categoric… p             0.571
#>  3 cyl                  6 am                    1 categoric… n             3    
#>  4 cyl                  6 am                    1 categoric… p             0.429
#>  5 cyl                  4 am                    0 categoric… n             3    
#>  6 cyl                  4 am                    0 categoric… p             0.273
#>  7 cyl                  4 am                    1 categoric… n             8    
#>  8 cyl                  4 am                    1 categoric… p             0.727
#>  9 cyl                  8 am                    0 categoric… n            12    
#> 10 cyl                  8 am                    0 categoric… p             0.857
#> # ℹ 36 more rows

ard_ttest(data = mtcars, by = "am", variable = "hp") |> 
  # convert list columns to character for a nicer print
  dplyr::mutate(across(where(is.list), ~lapply(., \(x) if (!is.null(x)) x else NA) |> unlist()))
#> # A tibble: 11 × 6
#>    strata1 variable context stat_name   statistic               strata1_level
#>    <chr>   <chr>    <chr>   <chr>       <chr>                           <dbl>
#>  1 am      hp       ttest   estimate    33.417004048583                    NA
#>  2 am      hp       ttest   estimate1   160.263157894737                    0
#>  3 am      hp       ttest   estimate2   126.846153846154                    1
#>  4 am      hp       ttest   statistic   1.26618876980934                   NA
#>  5 am      hp       ttest   p.value     0.220979581335913                  NA
#>  6 am      hp       ttest   parameter   18.7154096625045                   NA
#>  7 am      hp       ttest   conf.low    -21.8785802016468                  NA
#>  8 am      hp       ttest   conf.high   88.7125882988128                   NA
#>  9 am      hp       ttest   method      Welch Two Sample t-test            NA
#> 10 am      hp       ttest   alternative two.sided                          NA
#> 11 am      hp       ttest   conf.level  0.95                               NA
```
