
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

ard_continuous(ADSL, by = "ARM", variables = c("AGE", "BMIBL")) |> 
  flatten_ard() |> 
  head(n = 10) |> 
  knitr::kable()
```

| group1 | group1_level         | variable | stat_name | stat_label | statistic        | warning | error |
|:-------|:---------------------|:---------|:----------|:-----------|:-----------------|:--------|:------|
| ARM    | Placebo              | AGE      | N         | N          | 86               | NA      | NA    |
| ARM    | Placebo              | AGE      | length    | length     | 86               | NA      | NA    |
| ARM    | Placebo              | AGE      | mean      | Mean       | 75.2093023255814 | NA      | NA    |
| ARM    | Placebo              | AGE      | sd        | SD         | 8.59016712714193 | NA      | NA    |
| ARM    | Placebo              | AGE      | min       | Min        | 52               | NA      | NA    |
| ARM    | Placebo              | AGE      | max       | Max        | 89               | NA      | NA    |
| ARM    | Xanomeline High Dose | AGE      | N         | N          | 84               | NA      | NA    |
| ARM    | Xanomeline High Dose | AGE      | length    | length     | 84               | NA      | NA    |
| ARM    | Xanomeline High Dose | AGE      | mean      | Mean       | 74.3809523809524 | NA      | NA    |
| ARM    | Xanomeline High Dose | AGE      | sd        | SD         | 7.88609384869824 | NA      | NA    |

``` r

ard_categorical(ADSL, by = "ARM", variables = c("AGEGR1", "SEX")) |> 
  flatten_ard() |> 
  head(n = 10) |> 
  knitr::kable()
```

| group1 | group1_level         | variable | variable_level | stat_name | stat_label | statistic         | warning | error |
|:-------|:---------------------|:---------|:---------------|:----------|:-----------|:------------------|:--------|:------|
| ARM    | Placebo              | AGEGR1   | \<65           | n         | n          | 14                | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | \<65           | p         | %          | 0.162790697674419 | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | \>80           | n         | n          | 30                | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | \>80           | p         | %          | 0.348837209302326 | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | 65-80          | n         | n          | 42                | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | 65-80          | p         | %          | 0.488372093023256 | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | NA             | N         | N          | 86                | NA      | NA    |
| ARM    | Placebo              | AGEGR1   | NA             | length    | length     | 86                | NA      | NA    |
| ARM    | Xanomeline High Dose | AGEGR1   | \<65           | n         | n          | 11                | NA      | NA    |
| ARM    | Xanomeline High Dose | AGEGR1   | \<65           | p         | %          | 0.130952380952381 | NA      | NA    |

``` r

ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>  # only only two groups for a t-test
  ard_ttest(by = "ARM", variable = "AGE") |> 
  filter_ard(stat_name = c("estimate", "conf.low", "conf.high", "p.value")) |> 
  flatten_ard() |> 
  knitr::kable()
```

| group1 | group1_level | variable | stat_name | statistic         | warning | error |
|:-------|:-------------|:---------|:----------|:------------------|:--------|:------|
| ARM    | NA           | AGE      | estimate  | 0.828349944629011 | NA      | NA    |
| ARM    | NA           | AGE      | p.value   | 0.513240888362863 | NA      | NA    |
| ARM    | NA           | AGE      | conf.low  | -1.66763676468001 | NA      | NA    |
| ARM    | NA           | AGE      | conf.high | 3.32433665393803  | NA      | NA    |

``` r

# the example below ignores the time to event nature of the outcome
# and are provided to illustrate functionality only
glm(abs(CNSR - 1) ~ TRTP, data = ADTTE, family = binomial) |>
  ard_regression(add_estimate_to_reference_rows = TRUE) |> 
  filter_ard(stat_name = c("estimate", "conf.low", "conf.high", "p.value")) |> 
  flatten_ard() |> 
  tidyr::drop_na() |>
  knitr::kable()
```

| variable | variable_level       | stat_name | statistic            |
|:---------|:---------------------|:----------|:---------------------|
| TRTP     | Placebo              | estimate  | 0                    |
| TRTP     | Xanomeline High Dose | estimate  | 1.65113508608955     |
| TRTP     | Xanomeline High Dose | p.value   | 7.97602603440836e-07 |
| TRTP     | Xanomeline High Dose | conf.low  | 1.0082797171968      |
| TRTP     | Xanomeline High Dose | conf.high | 2.32276375858544     |
| TRTP     | Xanomeline Low Dose  | estimate  | 1.71184736953176     |
| TRTP     | Xanomeline Low Dose  | p.value   | 3.79931423922493e-07 |
| TRTP     | Xanomeline Low Dose  | conf.low  | 1.0647564793032      |
| TRTP     | Xanomeline Low Dose  | conf.high | 2.389373240016       |
