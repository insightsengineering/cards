# ard_categorical_max() works with default settings

    Code
      print(res, n = 20, columns = "all")
    Message
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fn warning error
      1    TRTA      Placebo    AESEV           MILD categori…         n          n    36      0              
      2    TRTA      Placebo    AESEV           MILD categori…         N          N    69      0              
      3    TRTA      Placebo    AESEV           MILD categori…         p          % 0.522   <fn>              
      4    TRTA      Placebo    AESEV       MODERATE categori…         n          n    26      0              
      5    TRTA      Placebo    AESEV       MODERATE categori…         N          N    69      0              
      6    TRTA      Placebo    AESEV       MODERATE categori…         p          % 0.377   <fn>              
      7    TRTA      Placebo    AESEV         SEVERE categori…         n          n     7      0              
      8    TRTA      Placebo    AESEV         SEVERE categori…         N          N    69      0              
      9    TRTA      Placebo    AESEV         SEVERE categori…         p          % 0.101   <fn>              
      10   TRTA    Xanomeli…    AESEV           MILD categori…         n          n    22      0              
      11   TRTA    Xanomeli…    AESEV           MILD categori…         N          N    79      0              
      12   TRTA    Xanomeli…    AESEV           MILD categori…         p          % 0.278   <fn>              
      13   TRTA    Xanomeli…    AESEV       MODERATE categori…         n          n    49      0              
      14   TRTA    Xanomeli…    AESEV       MODERATE categori…         N          N    79      0              
      15   TRTA    Xanomeli…    AESEV       MODERATE categori…         p          %  0.62   <fn>              
      16   TRTA    Xanomeli…    AESEV         SEVERE categori…         n          n     8      0              
      17   TRTA    Xanomeli…    AESEV         SEVERE categori…         N          N    79      0              
      18   TRTA    Xanomeli…    AESEV         SEVERE categori…         p          % 0.101   <fn>              
      19   TRTA    Xanomeli…    AESEV           MILD categori…         n          n    19      0              
      20   TRTA    Xanomeli…    AESEV           MILD categori…         N          N    77      0              
    Message
      i 7 more rows
      i Use `print(n = ...)` to see more rows

---

    Code
      print(ard_categorical_max(dplyr::group_by(cards::ADAE, TRTA), variables = AESEV, id = USUBJID, denominator = dplyr::rename(cards::ADSL, TRTA = ARM)), n = 20, columns = "all")
    Message
      `AESEV`: "MILD" < "MODERATE" < "SEVERE"
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fn warning error
      1    TRTA      Placebo    AESEV           MILD categori…         n          n    36      0              
      2    TRTA      Placebo    AESEV           MILD categori…         N          N    86      0              
      3    TRTA      Placebo    AESEV           MILD categori…         p          % 0.419   <fn>              
      4    TRTA      Placebo    AESEV       MODERATE categori…         n          n    26      0              
      5    TRTA      Placebo    AESEV       MODERATE categori…         N          N    86      0              
      6    TRTA      Placebo    AESEV       MODERATE categori…         p          % 0.302   <fn>              
      7    TRTA      Placebo    AESEV         SEVERE categori…         n          n     7      0              
      8    TRTA      Placebo    AESEV         SEVERE categori…         N          N    86      0              
      9    TRTA      Placebo    AESEV         SEVERE categori…         p          % 0.081   <fn>              
      10   TRTA    Xanomeli…    AESEV           MILD categori…         n          n    22      0              
      11   TRTA    Xanomeli…    AESEV           MILD categori…         N          N    84      0              
      12   TRTA    Xanomeli…    AESEV           MILD categori…         p          % 0.262   <fn>              
      13   TRTA    Xanomeli…    AESEV       MODERATE categori…         n          n    49      0              
      14   TRTA    Xanomeli…    AESEV       MODERATE categori…         N          N    84      0              
      15   TRTA    Xanomeli…    AESEV       MODERATE categori…         p          % 0.583   <fn>              
      16   TRTA    Xanomeli…    AESEV         SEVERE categori…         n          n     8      0              
      17   TRTA    Xanomeli…    AESEV         SEVERE categori…         N          N    84      0              
      18   TRTA    Xanomeli…    AESEV         SEVERE categori…         p          % 0.095   <fn>              
      19   TRTA    Xanomeli…    AESEV           MILD categori…         n          n    19      0              
      20   TRTA    Xanomeli…    AESEV           MILD categori…         N          N    84      0              
    Message
      i 7 more rows
      i Use `print(n = ...)` to see more rows

# ard_categorical_max(statistic) works

    Code
      ard_categorical_max(cards::ADAE, variables = AESEV, id = USUBJID, by = TRTA, denominator = dplyr::rename(cards::ADSL, TRTA = ARM), statistic = ~"n")
    Message
      `AESEV`: "MILD" < "MODERATE" < "SEVERE"
      {cards} data frame: 9 x 11
    Output
        group1 group1_level variable variable_level stat_name stat_label stat
      1   TRTA      Placebo    AESEV           MILD         n          n   36
      2   TRTA      Placebo    AESEV       MODERATE         n          n   26
      3   TRTA      Placebo    AESEV         SEVERE         n          n    7
      4   TRTA    Xanomeli…    AESEV           MILD         n          n   22
      5   TRTA    Xanomeli…    AESEV       MODERATE         n          n   49
      6   TRTA    Xanomeli…    AESEV         SEVERE         n          n    8
      7   TRTA    Xanomeli…    AESEV           MILD         n          n   19
      8   TRTA    Xanomeli…    AESEV       MODERATE         n          n   42
      9   TRTA    Xanomeli…    AESEV         SEVERE         n          n   16
    Message
      i 4 more variables: context, fmt_fn, warning, error

# ard_categorical_max(denominator) works

    Code
      ard_categorical_max(cards::ADAE, variables = AESEV, id = USUBJID, by = TRTA)
    Message
      `AESEV`: "MILD" < "MODERATE" < "SEVERE"
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level stat_name stat_label  stat
      1    TRTA      Placebo    AESEV           MILD         n          n    36
      2    TRTA      Placebo    AESEV           MILD         N          N    69
      3    TRTA      Placebo    AESEV           MILD         p          % 0.522
      4    TRTA      Placebo    AESEV       MODERATE         n          n    26
      5    TRTA      Placebo    AESEV       MODERATE         N          N    69
      6    TRTA      Placebo    AESEV       MODERATE         p          % 0.377
      7    TRTA      Placebo    AESEV         SEVERE         n          n     7
      8    TRTA      Placebo    AESEV         SEVERE         N          N    69
      9    TRTA      Placebo    AESEV         SEVERE         p          % 0.101
      10   TRTA    Xanomeli…    AESEV           MILD         n          n    22
    Message
      i 17 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fn, warning, error

---

    Code
      ard_categorical_max(cards::ADAE, variables = AESEV, id = USUBJID, by = TRTA, denominator = 100)
    Message
      `AESEV`: "MILD" < "MODERATE" < "SEVERE"
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level stat_name stat_label stat
      1    TRTA      Placebo    AESEV           MILD         n          n   36
      2    TRTA      Placebo    AESEV           MILD         N          N  100
      3    TRTA      Placebo    AESEV           MILD         p          % 0.36
      4    TRTA      Placebo    AESEV       MODERATE         n          n   26
      5    TRTA      Placebo    AESEV       MODERATE         N          N  100
      6    TRTA      Placebo    AESEV       MODERATE         p          % 0.26
      7    TRTA      Placebo    AESEV         SEVERE         n          n    7
      8    TRTA      Placebo    AESEV         SEVERE         N          N  100
      9    TRTA      Placebo    AESEV         SEVERE         p          % 0.07
      10   TRTA    Xanomeli…    AESEV           MILD         n          n   22
    Message
      i 17 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: context, fmt_fn, warning, error

# ard_categorical_max() works with pre-ordered factor variables

    Code
      print(res, n = 20, columns = "all")
    Message
      {cards} data frame: 27 x 11
    Output
         group1 group1_level variable variable_level   context stat_name stat_label  stat fmt_fn warning error
      1    TRTA      Placebo    AESEV           MILD categori…         n          n    36      0              
      2    TRTA      Placebo    AESEV           MILD categori…         N          N    86      0              
      3    TRTA      Placebo    AESEV           MILD categori…         p          % 0.419   <fn>              
      4    TRTA      Placebo    AESEV       MODERATE categori…         n          n    26      0              
      5    TRTA      Placebo    AESEV       MODERATE categori…         N          N    86      0              
      6    TRTA      Placebo    AESEV       MODERATE categori…         p          % 0.302   <fn>              
      7    TRTA      Placebo    AESEV         SEVERE categori…         n          n     7      0              
      8    TRTA      Placebo    AESEV         SEVERE categori…         N          N    86      0              
      9    TRTA      Placebo    AESEV         SEVERE categori…         p          % 0.081   <fn>              
      10   TRTA    Xanomeli…    AESEV           MILD categori…         n          n    22      0              
      11   TRTA    Xanomeli…    AESEV           MILD categori…         N          N    84      0              
      12   TRTA    Xanomeli…    AESEV           MILD categori…         p          % 0.262   <fn>              
      13   TRTA    Xanomeli…    AESEV       MODERATE categori…         n          n    49      0              
      14   TRTA    Xanomeli…    AESEV       MODERATE categori…         N          N    84      0              
      15   TRTA    Xanomeli…    AESEV       MODERATE categori…         p          % 0.583   <fn>              
      16   TRTA    Xanomeli…    AESEV         SEVERE categori…         n          n     8      0              
      17   TRTA    Xanomeli…    AESEV         SEVERE categori…         N          N    84      0              
      18   TRTA    Xanomeli…    AESEV         SEVERE categori…         p          % 0.095   <fn>              
      19   TRTA    Xanomeli…    AESEV           MILD categori…         n          n    19      0              
      20   TRTA    Xanomeli…    AESEV           MILD categori…         N          N    84      0              
    Message
      i 7 more rows
      i Use `print(n = ...)` to see more rows

# ard_categorical_max() errors with incomplete factor columns

    Code
      ard_categorical_max(dplyr::mutate(cards::ADAE, AESOC = factor(AESOC, levels = character(
        0))), variables = AESOC, id = USUBJID, by = TRTA)
    Condition
      Error in `ard_categorical_max()`:
      ! Factors with empty "levels" attribute are not allowed, which was identified in column "AESOC".

---

    Code
      ard_categorical_max(dplyr::mutate(cards::ADAE, SEX = factor(SEX, levels = c("F",
        "M", NA), exclude = NULL)), variables = SEX, id = USUBJID, by = TRTA)
    Condition
      Error in `ard_categorical_max()`:
      ! Factors with NA levels are not allowed, which are present in column "SEX".

# ard_categorical_max() works without any variables

    Code
      ard_categorical_max(data = cards::ADAE, variables = starts_with("xxxx"), id = USUBJID,
      by = c(TRTA, AESEV))
    Message
      {cards} data frame: 0 x 0
    Output
      data frame with 0 columns and 0 rows

