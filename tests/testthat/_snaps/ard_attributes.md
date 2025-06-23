# ard_attributes() works

    Code
      df <- dplyr::tibble(var1 = letters, var2 = LETTERS)
      attr(df$var1, "label") <- "Lowercase Letters"
      as.data.frame(ard_attributes(df, variables = everything(), label = list(var2 = "UPPERCASE LETTERS")))
    Output
        variable    context stat_name     stat_label              stat                    fmt_fun warning error
      1     var1 attributes     label Variable Label Lowercase Letters .Primitive("as.character")    NULL  NULL
      2     var1 attributes     class Variable Class         character                       NULL    NULL  NULL
      3     var2 attributes     label Variable Label UPPERCASE LETTERS .Primitive("as.character")    NULL  NULL
      4     var2 attributes     class Variable Class         character                       NULL    NULL  NULL

# ard_attributes() requires label as a named list

    Code
      ard_attributes(ADSL[c("AGE", "AGEGR1")], label = list("test"))
    Condition
      Error in `ard_attributes()`:
      ! The `label` argument must be a named list with each element a string.

