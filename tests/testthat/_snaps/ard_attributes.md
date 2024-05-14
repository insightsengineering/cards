# ard_attributes() works

    Code
      df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
      attr(df$var1, "label") <- "Lowercase Letters"
      as.data.frame(ard_attributes(df, variables = everything(), label = list(var2 = "UPPERCASE LETTERS")))
    Output
        variable    context stat_name     stat_label              stat
      1     var1 attributes     label Variable Label Lowercase Letters
      2     var1 attributes     class Variable Class         character
      3     var2 attributes     label Variable Label UPPERCASE LETTERS
      4     var2 attributes     class Variable Class         character

