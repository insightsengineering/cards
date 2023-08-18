
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cards

<!-- badges: start -->
<!-- [![R-CMD-check](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/insightsengineering/cards/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cards?branch=main) -->
<!-- badges: end -->

**This package is in a preliminary state, and breaking change will be
made without notice or deprecation.**

The {cards} package creates **C**DISC **A**nalysis **R**esult **D**ata
**S**ets.

## Installation

You can install the development version of cards from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("insightsengineering/cards")
```

## Example

ARD Examples

``` r
library(cards)
library(dplyr) |> suppressPackageStartupMessages()

ard_continuous(mtcars, by = "cyl", variables = c("mpg", "hp")) |> 
  flatten_ard()
#> # A tibble: 42 × 10
#>    strata1 strata1_level variable stat_name stat_label stat_format_fn  statistic
#>    <chr>   <chr>         <chr>    <chr>     <chr>      <chr>           <chr>    
#>  1 cyl     4             mpg      N         N          "function (x) … 11       
#>  2 cyl     4             mpg      N_miss    N Missing  "function (x) … 0        
#>  3 cyl     4             mpg      N_tot     Total N    "function (x) … 11       
#>  4 cyl     4             mpg      mean      Mean       "function (x) … 26.66363…
#>  5 cyl     4             mpg      sd        SD         "function (x) … 4.509827…
#>  6 cyl     4             mpg      min       Min        "function (x) … 21.4     
#>  7 cyl     4             mpg      max       Max        "function (x) … 33.9     
#>  8 cyl     4             hp       N         N          "function (x) … 11       
#>  9 cyl     4             hp       N_miss    N Missing  "function (x) … 0        
#> 10 cyl     4             hp       N_tot     Total N    "function (x) … 11       
#> # ℹ 32 more rows
#> # ℹ 3 more variables: warning <chr>, error <chr>, context <chr>

ard_categorical(mtcars, by = "cyl", variables = c("am", "gear")) |> 
  flatten_ard()
#> # A tibble: 48 × 11
#>    strata1 strata1_level variable stat_label stat_format_fn       variable_level
#>    <chr>   <chr>         <chr>    <chr>      <chr>                <chr>         
#>  1 cyl     4             am       table      "function (x) \nfor… 0             
#>  2 cyl     4             am       table      "function (x) \nfor… 0             
#>  3 cyl     4             am       table      "function (x) \nfor… 1             
#>  4 cyl     4             am       table      "function (x) \nfor… 1             
#>  5 cyl     6             am       table      "function (x) \nfor… 0             
#>  6 cyl     6             am       table      "function (x) \nfor… 0             
#>  7 cyl     6             am       table      "function (x) \nfor… 1             
#>  8 cyl     6             am       table      "function (x) \nfor… 1             
#>  9 cyl     8             am       table      "function (x) \nfor… 0             
#> 10 cyl     8             am       table      "function (x) \nfor… 0             
#> # ℹ 38 more rows
#> # ℹ 5 more variables: warning <chr>, error <chr>, context <chr>,
#> #   stat_name <chr>, statistic <chr>

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

<!-- ARD  -> Table Example -->
<!-- ```{r} -->
<!-- # Construct the ARD -->
<!-- table_ard <- -->
<!--   bind_rows( -->
<!--     ard_continuous(mtcars, by = cyl, variables = "mpg"), -->
<!--     ard_categorical(mtcars, by = cyl, variables = "am"), -->
<!--     ard_categorical(mtcars, variables = "cyl") -->
<!--   ) -->
<!-- # convert ARD to a cards table -->
<!-- table <- -->
<!--   construct_cards( -->
<!--     table_plan = -->
<!--       bind_rows( -->
<!--         table_ard |> filter(variable %in% "mpg") |>  table_plan_simple_continuous(), -->
<!--         table_ard |> filter(variable %in% "am") |> table_plan_simple_categorical() -->
<!--       ), -->
<!--     header_plan = -->
<!--       table_ard |> -->
<!--       filter(variable %in% "cyl") |> -->
<!--       header_plan_simple(header = "**{group} Cylinders**  \nN={n}  ({p}%)") |> -->
<!--       modifyList(val = list(label = gt::md("**Characteristic**"))) -->
<!--   ) |> -->
<!--   convert_cards(engine = "gt") -->
<!-- ``` -->
<!-- ```{r echo=FALSE, fig.width=4} -->
<!-- gt::gtsave(table, filename = "man/figures/README-table_example.png") -->
<!-- ``` -->
<!-- <img src="man/figures/README-table_example.png" style="width: 50%"> -->
