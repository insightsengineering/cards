
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cards <a href="https://insightsengineering.github.io/cards/"><img src="man/figures/logo.png" align="right" height="138" alt="cards website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/cards/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cards?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

## Extensions <a href="https://insightsengineering.github.io/cardx/"><img src="https://raw.githubusercontent.com/insightsengineering/cardx/main/man/figures/logo.png" align="right" height="138" alt="cards website" /></a>

The {cards} package exports three types of functions:

1.  Functions to create basic ARD objects.

2.  Utilities to create new ARD objects.

3.  Functions to work with existing ARD objects.

The [{cardx}](https://github.com/insightsengineering/cardx) R package is
an extension to {cards} that utilizes the utilities from {cards} and
exports functions for creating additional ARD objects––including
functions to summarize t-tests, Wilcoxon Rank-Sum tests, regression
models, and more.

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
#> {cards} data frame: 45 x 11
#>    group1 group1_level variable variable_level stat_name stat_label statistic
#> 1     ARM      Placebo   AGEGR1            <65         n          n        14
#> 2     ARM      Placebo   AGEGR1            <65         N          N        86
#> 3     ARM      Placebo   AGEGR1            <65         p          %     0.163
#> 4     ARM    Xanomeli…   AGEGR1            <65         n          n        11
#> 5     ARM    Xanomeli…   AGEGR1            <65         N          N        84
#> 6     ARM    Xanomeli…   AGEGR1            <65         p          %     0.131
#> 7     ARM    Xanomeli…   AGEGR1            <65         n          n         8
#> 8     ARM    Xanomeli…   AGEGR1            <65         N          N        84
#> 9     ARM    Xanomeli…   AGEGR1            <65         p          %     0.095
#> 10    ARM      Placebo   AGEGR1            >80         n          n        30
#> ℹ 35 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 4 more variables: context, statistic_fmt_fn, warning, error
```
