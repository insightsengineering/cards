
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
  flatten_ard()
#> # A tibble: 42 × 6
#>    strata1 strata1_levels variable stat_name statistic        context   
#>    <chr>            <dbl> <chr>    <chr>     <chr>            <chr>     
#>  1 cyl                  6 mpg      N         7                continuous
#>  2 cyl                  6 mpg      N_miss    0                continuous
#>  3 cyl                  6 mpg      N_tot     7                continuous
#>  4 cyl                  6 mpg      mean      19.7428571428571 continuous
#>  5 cyl                  6 mpg      sd        1.45356704106042 continuous
#>  6 cyl                  6 mpg      min       17.8             continuous
#>  7 cyl                  6 mpg      max       21.4             continuous
#>  8 cyl                  6 hp       N         7                continuous
#>  9 cyl                  6 hp       N_miss    0                continuous
#> 10 cyl                  6 hp       N_tot     7                continuous
#> # ℹ 32 more rows

ard_categorical(mtcars, by = cyl, include = c(am, gear)) |> 
  flatten_ard()
#> # A tibble: 46 × 7
#>    strata1 strata1_levels variable variable_level context    stat_name statistic
#>    <chr>            <dbl> <chr>    <chr>          <chr>      <chr>     <chr>    
#>  1 cyl                  6 am       0              categoric… n         4        
#>  2 cyl                  6 am       0              categoric… p         0.571428…
#>  3 cyl                  6 am       1              categoric… n         3        
#>  4 cyl                  6 am       1              categoric… p         0.428571…
#>  5 cyl                  4 am       0              categoric… n         3        
#>  6 cyl                  4 am       0              categoric… p         0.272727…
#>  7 cyl                  4 am       1              categoric… n         8        
#>  8 cyl                  4 am       1              categoric… p         0.727272…
#>  9 cyl                  8 am       0              categoric… n         12       
#> 10 cyl                  8 am       0              categoric… p         0.857142…
#> # ℹ 36 more rows

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
#> # A tibble: 68 × 4
#>    variable variable_level stat_name      statistic 
#>    <chr>    <chr>          <chr>          <chr>     
#>  1 mpg      <NA>           term           mpg       
#>  2 mpg      <NA>           var_label      mpg       
#>  3 mpg      <NA>           var_class      numeric   
#>  4 mpg      <NA>           var_type       continuous
#>  5 mpg      <NA>           var_nlevels    <NA>      
#>  6 mpg      <NA>           contrasts      <NA>      
#>  7 mpg      <NA>           contrasts_type <NA>      
#>  8 mpg      <NA>           reference_row  <NA>      
#>  9 mpg      <NA>           label          mpg       
#> 10 mpg      <NA>           n_obs          32        
#> # ℹ 58 more rows
```
