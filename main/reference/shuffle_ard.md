# Shuffle ARD

**\[experimental\]**  

This function ingests an ARD object and shuffles the information to
prepare for analysis. Helpful for streamlining across multiple ARDs.
Combines each group/group_level into 1 column, back fills missing
grouping values from the variable levels where possible, and optionally
trims statistics-level metadata.

## Usage

``` r
shuffle_ard(x, trim = TRUE)
```

## Arguments

- x:

  (`data.frame`)  
  an ARD data frame of class 'card'

- trim:

  (`logical`)  
  logical representing whether or not to trim away statistic-level
  metadata and filter only on numeric statistic values.

## Value

a tibble

## Examples

``` r
bind_ard(
  ard_tabulate(ADSL, by = "ARM", variables = "AGEGR1"),
  ard_tabulate(ADSL, variables = "ARM")
) |>
  shuffle_ard()
#> # A tibble: 36 × 7
#>    ARM               variable variable_level context stat_name stat_label   stat
#>    <chr>             <chr>    <chr>          <chr>   <chr>     <chr>       <dbl>
#>  1 Placebo           AGEGR1   65-80          tabula… n         n          42    
#>  2 Placebo           AGEGR1   65-80          tabula… N         N          86    
#>  3 Placebo           AGEGR1   65-80          tabula… p         %           0.488
#>  4 Placebo           AGEGR1   <65            tabula… n         n          14    
#>  5 Placebo           AGEGR1   <65            tabula… N         N          86    
#>  6 Placebo           AGEGR1   <65            tabula… p         %           0.163
#>  7 Placebo           AGEGR1   >80            tabula… n         n          30    
#>  8 Placebo           AGEGR1   >80            tabula… N         N          86    
#>  9 Placebo           AGEGR1   >80            tabula… p         %           0.349
#> 10 Xanomeline High … AGEGR1   65-80          tabula… n         n          55    
#> # ℹ 26 more rows
```
