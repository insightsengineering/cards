# tidy_ard_row_order() works

    Code
      dplyr::select(ard_categorical(data.frame(x1 = sample(LETTERS[1:5], 30, replace = TRUE), x2 = sample(LETTERS[6:10], 30,
      replace = TRUE), x3 = sample(LETTERS[11:15], 30, replace = TRUE), zz = 1L, aa = 1L), by = x1:x3, variables = c(zz, aa),
      statistic = everything() ~ "n"), all_ard_groups(), all_ard_variables())
    Message
      {cards} data frame: 250 x 8
    Output
         group3 group3_level group2 group2_level group1 group1_level variable variable_level
      1      x3            K     x2            F     x1            A       zz              1
      2      x3            K     x2            F     x1            A       aa              1
      3      x3            L     x2            F     x1            A       zz              1
      4      x3            L     x2            F     x1            A       aa              1
      5      x3            M     x2            F     x1            A       zz              1
      6      x3            M     x2            F     x1            A       aa              1
      7      x3            N     x2            F     x1            A       zz              1
      8      x3            N     x2            F     x1            A       aa              1
      9      x3            O     x2            F     x1            A       zz              1
      10     x3            O     x2            F     x1            A       aa              1
    Message
      i 240 more rows
      i Use `print(n = ...)` to see more rows

