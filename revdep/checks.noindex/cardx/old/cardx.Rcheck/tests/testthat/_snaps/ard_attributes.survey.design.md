# ard_attributes.survey.design() works

    Code
      attr(dclus1$variables$sname, "label") <- "School Name"
      as.data.frame(ard_attributes(dclus1, variables = c(sname, dname), label = list(dname = "District Name")))
    Output
        variable    context stat_name     stat_label          stat                     fmt_fn warning error
      1    sname attributes     label Variable Label   School Name .Primitive("as.character")    NULL  NULL
      2    sname attributes     class Variable Class     character                       NULL    NULL  NULL
      3    dname attributes     label Variable Label District Name .Primitive("as.character")    NULL  NULL
      4    dname attributes     class Variable Class     character                       NULL    NULL  NULL

