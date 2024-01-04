
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
#>    group1 group1_level variable   context stat_name      stat_label statistic
#> 1     ARM      Placebo      AGE continuo…         N               N        86
#> 2     ARM      Placebo      AGE continuo…      mean            Mean    75.209
#> 3     ARM      Placebo      AGE continuo…        sd              SD      8.59
#> 4     ARM      Placebo      AGE continuo…    median          Median        76
#> 5     ARM      Placebo      AGE continuo…       p25 25th Percentile        69
#> 6     ARM      Placebo      AGE continuo…       p75 75th Percentile        82
#> 7     ARM      Placebo      AGE continuo…       min             Min        52
#> 8     ARM      Placebo      AGE continuo…       max             Max        89
#> 9     ARM      Placebo    BMIBL continuo…         N               N        86
#> 10    ARM      Placebo    BMIBL continuo…      mean            Mean    23.636
#> ℹ 38 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 3 more variables: statistic_fmt_fn, warning, error

ard_categorical(ADSL, by = "ARM", variables = c("AGEGR1", "SEX"))
#> {cards} data frame: 36 x 11
#>    group1 group1_level variable variable_level   context stat_name stat_label
#> 1     ARM      Placebo   AGEGR1            <65 categori…         n          n
#> 2     ARM      Placebo   AGEGR1            <65 categori…         p          %
#> 3     ARM      Placebo   AGEGR1            >80 categori…         n          n
#> 4     ARM      Placebo   AGEGR1            >80 categori…         p          %
#> 5     ARM      Placebo   AGEGR1          65-80 categori…         n          n
#> 6     ARM      Placebo   AGEGR1          65-80 categori…         p          %
#> 7     ARM      Placebo   AGEGR1                categori…         N          N
#> 8     ARM      Placebo      SEX              F categori…         n          n
#> 9     ARM      Placebo      SEX              F categori…         p          %
#> 10    ARM      Placebo      SEX              M categori…         n          n
#>    statistic
#> 1         14
#> 2      0.163
#> 3         30
#> 4      0.349
#> 5         42
#> 6      0.488
#> 7         86
#> 8         53
#> 9      0.616
#> 10        33
#> ℹ 26 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 3 more variables: statistic_fmt_fn, warning, error

ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>  # only only two groups for a t-test
  ard_ttest(by = "ARM", variable = "AGE")
#> {cards} data frame: 14 x 10
#>    group1 group1_level variable context   stat_name          stat_label
#> 1     ARM                   AGE   ttest    estimate     Mean Difference
#> 2     ARM      Placebo      AGE   ttest   estimate1        Group 1 Mean
#> 3     ARM    Xanomeli…      AGE   ttest   estimate2        Group 2 Mean
#> 4     ARM                   AGE   ttest   statistic         t Statistic
#> 5     ARM                   AGE   ttest     p.value             p-value
#> 6     ARM                   AGE   ttest   parameter  Degrees of Freedom
#> 7     ARM                   AGE   ttest    conf.low      CI Lower Bound
#> 8     ARM                   AGE   ttest   conf.high      CI Upper Bound
#> 9     ARM                   AGE   ttest      method              method
#> 10    ARM                   AGE   ttest alternative         alternative
#> 11    ARM                   AGE   ttest          mu             H0 Mean
#> 12    ARM                   AGE   ttest      paired       Paired t-test
#> 13    ARM                   AGE   ttest   var.equal     Equal Variances
#> 14    ARM                   AGE   ttest  conf.level CI Confidence Level
#>    statistic
#> 1      0.828
#> 2     75.209
#> 3     74.381
#> 4      0.655
#> 5      0.513
#> 6    167.362
#> 7     -1.668
#> 8      3.324
#> 9  Welch Tw…
#> 10 two.sided
#> 11         0
#> 12     FALSE
#> 13     FALSE
#> 14      0.95
#> ℹ 3 more variables: statistic_fmt_fn, warning, error

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
