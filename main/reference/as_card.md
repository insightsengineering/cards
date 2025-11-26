# Data Frame as ARD

Convert data frames to ARDs of class 'card'.

## Usage

``` r
as_card(x)
```

## Arguments

- x:

  (`data.frame`)  
  a data frame

## Value

an ARD data frame of class 'card'

## Examples

``` r
data.frame(
  stat_name = c("N", "mean"),
  stat_label = c("N", "Mean"),
  stat = c(10, 0.5)
) |>
  as_card()
#> {cards} data frame: 2 x 3
#>   stat_name stat_label stat
#> 1         N          N   10
#> 2      mean       Mean  0.5
```
