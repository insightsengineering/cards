# rename_ard_groups_shift()

    Code
      dplyr::select(rename_ard_groups_shift(ard_continuous(ADSL, variables = AGE, by = c(
        SEX, ARM)), shift = 1L), all_ard_groups()) %>% 1L[]
    Message
      {cards} data frame: 1 x 4
    Output
        group2 group2_level group3 group3_level
      1    SEX            F    ARM      Placebo

# rename_ard_groups_shift() messaging

    Code
      dplyr::select(rename_ard_groups_shift(ard_continuous(ADSL, variables = AGE, by = c(
        SEX, ARM)), shift = -1L), all_ard_groups()) %>% 1L[]
    Message
      There are now non-standard group column names: "group0" and "group0_level".
      i Is this the shift you had planned?
      {cards} data frame: 1 x 4
    Output
        group0 group0_level group1 group1_level
      1    SEX            F    ARM      Placebo

# rename_ard_groups_reverse()

    Code
      dplyr::select(rename_ard_groups_reverse(ard_continuous(ADSL, variables = AGE,
        by = c(SEX, ARM))), all_ard_groups()) %>% 1L[]
    Message
      {cards} data frame: 1 x 4
    Output
        group1 group1_level group2 group2_level
      1    ARM      Placebo    SEX            F

