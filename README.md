
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cards

<!-- badges: start -->

[![R-CMD-check](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/cards/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cards?branch=main)
<!-- badges: end -->

This package is in a **preliminary state**, and **breaking changes**
will be made **without notice or deprecation**.

The [CDISC Analysis Results
Standard](https://www.cdisc.org/standards/foundational/analysis-results-standards)
aims to facilitate automation, reproducibility, reusability, and
traceability of analysis results data (ARD). The {cards} package creates
these **C**DISC **A**nalysis **R**esult **D**ata **S**ets.

Use cases:

1.  Quality Control (QC) of existing tables and figures.

2.  Pre-calculate statistics to be summarized in tables and figures.

3.  Medical writers may easily access statistics and place in reports
    without copying and pasting from reports.

4.  Provides a consistent format for results and lends results to be
    combined across studies for re-use and re-analysis.

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

ard_continuous(ADSL, by = "ARM", variables = c("AGE", "BMIBL"))
#> {cards} data frame: 48 x 10
#>    group1 group1_level variable stat_name stat_label statistic
#> 1     ARM      Placebo      AGE         N          N        86
#> 2     ARM      Placebo      AGE      mean       Mean    75.209
#> 3     ARM      Placebo      AGE        sd         SD      8.59
#> 4     ARM      Placebo      AGE    median     Median        76
#> 5     ARM      Placebo      AGE       p25  25th Per…        69
#> 6     ARM      Placebo      AGE       p75  75th Per…        82
#> 7     ARM      Placebo      AGE       min        Min        52
#> 8     ARM      Placebo      AGE       max        Max        89
#> 9     ARM      Placebo    BMIBL         N          N        86
#> 10    ARM      Placebo    BMIBL      mean       Mean    23.636
#> ℹ 38 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, statistic_fmt_fn, warning, error

ard_categorical(ADSL, by = "ARM", variables = c("AGEGR1", "SEX"))
#> {cards} data frame: 36 x 11
#>    group1 group1_level variable variable_level stat_name stat_label statistic
#> 1     ARM      Placebo   AGEGR1            <65         n          n        14
#> 2     ARM      Placebo   AGEGR1            <65         p          %     0.163
#> 3     ARM      Placebo   AGEGR1            >80         n          n        30
#> 4     ARM      Placebo   AGEGR1            >80         p          %     0.349
#> 5     ARM      Placebo   AGEGR1          65-80         n          n        42
#> 6     ARM      Placebo   AGEGR1          65-80         p          %     0.488
#> 7     ARM      Placebo   AGEGR1                        N          N        86
#> 8     ARM      Placebo      SEX              F         n          n        53
#> 9     ARM      Placebo      SEX              F         p          %     0.616
#> 10    ARM      Placebo      SEX              M         n          n        33
#> ℹ 26 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, statistic_fmt_fn, warning, error

ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>  # only only two groups for a t-test
  ard_ttest(by = "ARM", variable = "AGE")
#> {cards} data frame: 14 x 10
#>    group1 group1_level variable   stat_name stat_label statistic
#> 1     ARM                   AGE    estimate  Mean Dif…     0.828
#> 2     ARM      Placebo      AGE   estimate1  Group 1 …    75.209
#> 3     ARM    Xanomeli…      AGE   estimate2  Group 2 …    74.381
#> 4     ARM                   AGE   statistic  t Statis…     0.655
#> 5     ARM                   AGE     p.value    p-value     0.513
#> 6     ARM                   AGE   parameter  Degrees …   167.362
#> 7     ARM                   AGE    conf.low  CI Lower…    -1.668
#> 8     ARM                   AGE   conf.high  CI Upper…     3.324
#> 9     ARM                   AGE      method     method Welch Tw…
#> 10    ARM                   AGE alternative  alternat… two.sided
#> 11    ARM                   AGE          mu    H0 Mean         0
#> 12    ARM                   AGE      paired  Paired t…     FALSE
#> 13    ARM                   AGE   var.equal  Equal Va…     FALSE
#> 14    ARM                   AGE  conf.level  CI Confi…      0.95
#> ℹ 4 more variables: context, statistic_fmt_fn, warning, error

survival::coxph(ggsurvfit::Surv_CNSR() ~ TRTP, data = ADTTE) |>
  ard_regression(add_estimate_to_reference_rows = TRUE) |> 
  dplyr::filter(stat_name %in% c("estimate", "conf.low", "conf.high", "p.value"))
#> {cards} data frame: 12 x 6
#>    variable variable_level   context stat_name statistic statistic_fmt_fn
#> 1      TRTP        Placebo regressi…  estimate         0                1
#> 2      TRTP        Placebo regressi…   p.value        NA                1
#> 3      TRTP        Placebo regressi…  conf.low        NA                1
#> 4      TRTP        Placebo regressi… conf.high        NA                1
#> 5      TRTP      Xanomeli… regressi…  estimate     1.615                1
#> 6      TRTP      Xanomeli… regressi…   p.value         0                1
#> 7      TRTP      Xanomeli… regressi…  conf.low     1.157                1
#> 8      TRTP      Xanomeli… regressi… conf.high     2.072                1
#> 9      TRTP      Xanomeli… regressi…  estimate     1.423                1
#> 10     TRTP      Xanomeli… regressi…   p.value         0                1
#> 11     TRTP      Xanomeli… regressi…  conf.low     0.973                1
#> 12     TRTP      Xanomeli… regressi… conf.high     1.872                1
```
