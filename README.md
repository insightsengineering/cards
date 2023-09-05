
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
  flatten_ard() |> 
  head(n = 10) |> 
  knitr::kable()
```

| group1 | group1_level         | variable | stat_name   | statistic               | warning | error |
|:-------|:---------------------|:---------|:------------|:------------------------|:--------|:------|
| ARM    | Placebo              | AGE      | estimate1   | 75.2093023255814        | NA      | NA    |
| ARM    | Xanomeline High Dose | AGE      | estimate2   | 74.3809523809524        | NA      | NA    |
| ARM    | NA                   | AGE      | estimate    | 0.828349944629011       | NA      | NA    |
| ARM    | NA                   | AGE      | statistic   | 0.655196351798793       | NA      | NA    |
| ARM    | NA                   | AGE      | p.value     | 0.513240888362863       | NA      | NA    |
| ARM    | NA                   | AGE      | parameter   | 167.362493715531        | NA      | NA    |
| ARM    | NA                   | AGE      | conf.low    | -1.66763676468001       | NA      | NA    |
| ARM    | NA                   | AGE      | conf.high   | 3.32433665393803        | NA      | NA    |
| ARM    | NA                   | AGE      | method      | Welch Two Sample t-test | NA      | NA    |
| ARM    | NA                   | AGE      | alternative | two.sided               | NA      | NA    |

``` r

# the example below ignores the time to event nature of the outcome
# and are provided to illustrate functionality only
glm(abs(CNSR - 1) ~ TRTP, data = ADTTE, family = binomial) |>
  ard_regression(add_estimate_to_reference_rows = TRUE) |> 
  flatten_ard() |> 
  dplyr::filter(variable_level %in% "Xanomeline High Dose") |> 
  tidyr::drop_na() |>
  knitr::kable()
```

| variable | variable_level       | stat_name      | statistic                |
|:---------|:---------------------|:---------------|:-------------------------|
| TRTP     | Xanomeline High Dose | term           | TRTPXanomeline High Dose |
| TRTP     | Xanomeline High Dose | var_label      | Planned Treatment        |
| TRTP     | Xanomeline High Dose | var_class      | character                |
| TRTP     | Xanomeline High Dose | var_type       | categorical              |
| TRTP     | Xanomeline High Dose | var_nlevels    | 3                        |
| TRTP     | Xanomeline High Dose | contrasts      | contr.treatment          |
| TRTP     | Xanomeline High Dose | contrasts_type | treatment                |
| TRTP     | Xanomeline High Dose | reference_row  | FALSE                    |
| TRTP     | Xanomeline High Dose | label          | Xanomeline High Dose     |
| TRTP     | Xanomeline High Dose | n_obs          | 84                       |
| TRTP     | Xanomeline High Dose | n_event        | 61                       |
| TRTP     | Xanomeline High Dose | estimate       | 1.65113508608955         |
| TRTP     | Xanomeline High Dose | std.error      | 0.334511905011521        |
| TRTP     | Xanomeline High Dose | statistic      | 4.93595313456089         |
| TRTP     | Xanomeline High Dose | p.value        | 7.97602603440836e-07     |
| TRTP     | Xanomeline High Dose | conf.low       | 1.0082797171968          |
| TRTP     | Xanomeline High Dose | conf.high      | 2.32276375858544         |
