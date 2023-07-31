
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

ard_continuous(mtcars, by = cyl, include = c(mpg, hp)) |> 
  flatten_ard()
#> # A tibble: 42 × 6
#>    strata1 strata1_level variable stat_name statistic        context   
#>    <chr>           <dbl> <chr>    <chr>     <chr>            <chr>     
#>  1 cyl                 6 mpg      N         7                continuous
#>  2 cyl                 6 mpg      N_miss    0                continuous
#>  3 cyl                 6 mpg      N_tot     7                continuous
#>  4 cyl                 6 mpg      mean      19.7428571428571 continuous
#>  5 cyl                 6 mpg      sd        1.45356704106042 continuous
#>  6 cyl                 6 mpg      min       17.8             continuous
#>  7 cyl                 6 mpg      max       21.4             continuous
#>  8 cyl                 6 hp       N         7                continuous
#>  9 cyl                 6 hp       N_miss    0                continuous
#> 10 cyl                 6 hp       N_tot     7                continuous
#> # ℹ 32 more rows

ard_categorical(mtcars, by = cyl, include = c(am, gear)) |> 
  flatten_ard()
#> # A tibble: 46 × 7
#>    strata1 strata1_level variable variable_level context     stat_name statistic
#>    <chr>           <dbl> <chr>    <chr>          <chr>       <chr>     <chr>    
#>  1 cyl                 6 am       0              categorical n         4        
#>  2 cyl                 6 am       0              categorical p         0.571428…
#>  3 cyl                 6 am       1              categorical n         3        
#>  4 cyl                 6 am       1              categorical p         0.428571…
#>  5 cyl                 4 am       0              categorical n         3        
#>  6 cyl                 4 am       0              categorical p         0.272727…
#>  7 cyl                 4 am       1              categorical n         8        
#>  8 cyl                 4 am       1              categorical p         0.727272…
#>  9 cyl                 8 am       0              categorical n         12       
#> 10 cyl                 8 am       0              categorical p         0.857142…
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
  dplyr::bind_rows(
    ard_continuous(mtcars, by = cyl, include = mpg),
    ard_categorical(mtcars, by = cyl, include = am),
    # TODO: The ARD creation code can by simplified after the categorical
    #       ARD accepts no-by variable specifications
    mtcars |>
      dplyr::mutate(..one.. = 1L) |>
      ard_categorical(by = ..one..,  include = cyl) |>
      dplyr::select(-starts_with("strata"))
  )

# convert ARD to a cardinal table
table <-
  construct_cardinal(
    plan_table =
      dplyr::bind_rows(
        table_ard |> dplyr::filter(variable %in% "mpg") |>  plan_table_simple_continuous(),
        table_ard |> dplyr::filter(variable %in% "am") |> plan_table_simple_categorical()
      ),
    plan_header =
      table_ard |>
      dplyr::filter(variable %in% "cyl") |>
      plan_header_simple(header = "**{strata} Cylinders  \nN={n}  ({p}%)**") |>
      modifyList(val = list(label = gt::md("**Characteristic**")))
  ) |>
  convert_cardinal(engine = "gt") |> 
  gt::text_transform(
    locations = gt::cells_body(
      columns = label,
      rows = !header_row
    ),
    fn = function(x) paste0("\U00A0\U00A0\U00A0\U00A0", x)
  ) |> 
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(
      columns = label,
      rows = header_row
    )
  )
```

<img src="man/figures/README-table_example.png" width="100%" />
