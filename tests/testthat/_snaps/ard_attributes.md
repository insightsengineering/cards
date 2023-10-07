# ard_attributes() works

    Code
      df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
      attr(df$var1, "label") <- "Lowercase Letters"
      as.data.frame(flatten_ard(ard_attributes(df, variables = everything(), label = list(
        var2 = "UPPERCASE LETTERS"))))
    Output
        variable stat_name     stat_label         statistic
      1     var1     label Variable Label Lowercase Letters
      2     var1     class Variable Class         character
      3     var2     label Variable Label UPPERCASE LETTERS
      4     var2     class Variable Class         character

