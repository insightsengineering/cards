# ard_attributes.survey.design() works

    Code
      attr(dclus1$variables$sname, "label") <- "School Name"
      as.data.frame(ard_attributes(dclus1, variables = c(sname, dname), label = list(
        dname = "District Name")))
    Output
        variable    context stat_name     stat_label          stat
      1    sname attributes     label Variable Label   School Name
      2    sname attributes     class Variable Class     character
      3    dname attributes     label Variable Label District Name
      4    dname attributes     class Variable Class     character

