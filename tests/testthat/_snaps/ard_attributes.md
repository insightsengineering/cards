# ard_attributes() works

    Code
      df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
      attr(df$var1, "label") <- "Lowercase Letters"
      as.data.frame(ard_attributes(df, variables = everything(), label = list(var2 = "UPPERCASE LETTERS")))
    Output
        variable stat_name              stat     stat_label    context
      1     var1     label Lowercase Letters Variable Label attributes
      2     var1     class         character Variable Class attributes
      3     var2     label UPPERCASE LETTERS Variable Label attributes
      4     var2     class         character Variable Class attributes

