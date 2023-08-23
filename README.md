
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cards

<!-- badges: start -->
<!-- [![R-CMD-check](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/insightsengineering/cards/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cards?branch=main) -->
<!-- badges: end -->

This package is in a **preliminary state**, and **breaking changes**
will be made **without notice or deprecation**.

The [CDISC Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standards)
aims to facilitate automation, reproducibility, reusability, and
traceability of analysis results data (ARD). The {cards} package creates
these **C**DISC **A**nalysis **R**esult **D**ata **S**ets.

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
  flatten_ard() |> 
  head(n = 10) |> 
  knitr::kable()
```

| group1 | group1_level | variable | stat_name | stat_label | statistic        | warning | error |
|:-------|:-------------|:---------|:----------|:-----------|:-----------------|:--------|:------|
| cyl    | 4            | hp       | N         | N          | 11               | NA      | NA    |
| cyl    | 4            | hp       | N_miss    | N Missing  | 0                | NA      | NA    |
| cyl    | 4            | hp       | N_tot     | Total N    | 11               | NA      | NA    |
| cyl    | 4            | hp       | mean      | Mean       | 82.6363636363636 | NA      | NA    |
| cyl    | 4            | hp       | sd        | SD         | 20.934529979308  | NA      | NA    |
| cyl    | 4            | hp       | min       | Min        | 52               | NA      | NA    |
| cyl    | 4            | hp       | max       | Max        | 113              | NA      | NA    |
| cyl    | 6            | hp       | N         | N          | 7                | NA      | NA    |
| cyl    | 6            | hp       | N_miss    | N Missing  | 0                | NA      | NA    |
| cyl    | 6            | hp       | N_tot     | Total N    | 7                | NA      | NA    |

``` r

ard_categorical(mtcars, by = "cyl", variables = c("am", "gear")) |> 
  flatten_ard() |> 
  head(n = 10) |> 
  knitr::kable()
```

| group1 | group1_level | variable | variable_level | stat_name | stat_label | statistic         | warning | error |
|:-------|:-------------|:---------|:---------------|:----------|:-----------|:------------------|:--------|:------|
| cyl    | 4            | am       | 0              | n         | n          | 3                 | NA      | NA    |
| cyl    | 4            | am       | 0              | p         | %          | 0.272727272727273 | NA      | NA    |
| cyl    | 4            | am       | 1              | n         | n          | 8                 | NA      | NA    |
| cyl    | 4            | am       | 1              | p         | %          | 0.727272727272727 | NA      | NA    |
| cyl    | 4            | am       | NA             | N         | N          | 11                | NA      | NA    |
| cyl    | 4            | am       | NA             | N_miss    | N Missing  | 0                 | NA      | NA    |
| cyl    | 4            | am       | NA             | N_tot     | Total N    | 11                | NA      | NA    |
| cyl    | 6            | am       | 0              | n         | n          | 4                 | NA      | NA    |
| cyl    | 6            | am       | 0              | p         | %          | 0.571428571428571 | NA      | NA    |
| cyl    | 6            | am       | 1              | n         | n          | 3                 | NA      | NA    |

``` r

ard_ttest(data = mtcars, by = "am", variable = "hp") |> 
  flatten_ard() |> 
  head(n = 10) |> 
  knitr::kable()
```

| group1 | group1_level | variable | stat_name   | statistic               | warning | error |
|:-------|:-------------|:---------|:------------|:------------------------|:--------|:------|
| am     | 0            | hp       | estimate1   | 160.263157894737        | NA      | NA    |
| am     | 1            | hp       | estimate2   | 126.846153846154        | NA      | NA    |
| am     | NA           | hp       | estimate    | 33.417004048583         | NA      | NA    |
| am     | NA           | hp       | statistic   | 1.26618876980934        | NA      | NA    |
| am     | NA           | hp       | p.value     | 0.220979581335913       | NA      | NA    |
| am     | NA           | hp       | parameter   | 18.7154096625045        | NA      | NA    |
| am     | NA           | hp       | conf.low    | -21.8785802016468       | NA      | NA    |
| am     | NA           | hp       | conf.high   | 88.7125882988128        | NA      | NA    |
| am     | NA           | hp       | method      | Welch Two Sample t-test | NA      | NA    |
| am     | NA           | hp       | alternative | two.sided               | NA      | NA    |

``` r

glm(am ~ factor(cyl), data = mtcars, family = binomial) |>
  ard_regression(add_estimate_to_reference_rows = TRUE) |> 
  flatten_ard() |> 
  dplyr::filter(variable_level %in% "8") |> 
  tidyr::drop_na() |> 
  knitr::kable()
```

| variable    | variable_level | stat_name      | statistic          |
|:------------|:---------------|:---------------|:-------------------|
| factor(cyl) | 8              | term           | factor(cyl)8       |
| factor(cyl) | 8              | var_label      | factor(cyl)        |
| factor(cyl) | 8              | var_class      | factor             |
| factor(cyl) | 8              | var_type       | categorical        |
| factor(cyl) | 8              | var_nlevels    | 3                  |
| factor(cyl) | 8              | contrasts      | contr.treatment    |
| factor(cyl) | 8              | contrasts_type | treatment          |
| factor(cyl) | 8              | reference_row  | FALSE              |
| factor(cyl) | 8              | label          | 8                  |
| factor(cyl) | 8              | estimate       | -2.77258872223715  |
| factor(cyl) | 8              | std.error      | 1.0206203034945    |
| factor(cyl) | 8              | statistic      | -2.71657217943254  |
| factor(cyl) | 8              | p.value        | 0.0065961810499368 |
| factor(cyl) | 8              | conf.low       | -5.04529335824973  |
| factor(cyl) | 8              | conf.high      | -0.93331910323904  |

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
