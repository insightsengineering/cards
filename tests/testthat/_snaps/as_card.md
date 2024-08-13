# as_card() works

    Code
      as_card(data.frame(stat_name = c("N", "mean"), stat_label = c("N", "Mean"),
      stat = c(10, 0.5)))
    Message
      {cards} data frame: 2 x 3
    Output
        stat_name stat_label stat
      1         N          N   10
      2      mean       Mean  0.5

# as_card() error catching works correctly

    Code
      as_card("notadataframe")
    Condition
      Error in `as_card()`:
      ! The `x` argument must be class <data.frame>, not a string.

