
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cards <a href="https://insightsengineering.github.io/cards/"><img src="man/figures/logo.png" align="right" height="138" alt="cards website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/insightsengineering/cards/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/cards/branch/main/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/cards?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

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

Install cards from CRAN with:

``` r
install.packages("cards")
```

You can install the development version of cards from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("insightsengineering/cards")
```

## Extensions

[<img
src="https://raw.githubusercontent.com/insightsengineering/cardx/main/man/figures/logo.png"
style="float: right" width="120" alt="cardx website" />](https://insightsengineering.github.io/cardx/)

The {cards} package exports three types of functions:

1.  Functions to create basic ARD objects.

2.  Utilities to create new ARD objects.

3.  Functions to work with existing ARD objects.

The [{cardx}](https://github.com/insightsengineering/cardx/) R package
is an extension to {cards} that uses the utilities from {cards} and
exports functions for creating additional ARD objects––including
functions to summarize t-tests, Wilcoxon Rank-Sum tests, regression
models, and more.

## Getting Started

Review the [Getting
Started](https://insightsengineering.github.io/cards/main/articles/getting-started.html)
page for examples using ARDs to calculate statistics to later include in
tables.
