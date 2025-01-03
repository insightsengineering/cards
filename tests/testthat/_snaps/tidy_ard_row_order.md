# tidy_ard_row_order() works

    Code
      dplyr::select(ard_categorical(data.frame(x1 = sample(LETTERS[1:5], 30, replace = TRUE),
      x2 = sample(LETTERS[6:10], 30, replace = TRUE), x3 = sample(LETTERS[11:15], 30,
      replace = TRUE), zz = 1L, aa = 1L), by = x1:x3, variables = c(zz, aa),
      statistic = everything() ~ "n"), all_ard_groups(), all_ard_variables())
    Message
      {cards} data frame: 250 x 8
    Output
         group1 group1_level group2 group2_level group3 group3_level variable
      1      x1            A     x2            F     x3            K       zz
      2      x1            A     x2            F     x3            K       aa
      3      x1            A     x2            F     x3            L       zz
      4      x1            A     x2            F     x3            L       aa
      5      x1            A     x2            F     x3            M       zz
      6      x1            A     x2            F     x3            M       aa
      7      x1            A     x2            F     x3            N       zz
      8      x1            A     x2            F     x3            N       aa
      9      x1            A     x2            F     x3            O       zz
      10     x1            A     x2            F     x3            O       aa
         variable_level
      1               1
      2               1
      3               1
      4               1
      5               1
      6               1
      7               1
      8               1
      9               1
      10              1
    Message
      i 240 more rows
      i Use `print(n = ...)` to see more rows

