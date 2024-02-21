# tidy_as_ard() works

    Code
      as.data.frame(tidy_as_ard(lst_tidy = eval_capture_conditions(dplyr::as_tibble(
        stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c("estimate",
          "p.value", "method")])), tidy_result_names = c("estimate", "p.value",
        "method"), fun_args_to_record = c("workspace", "hybrid", "hybridPars",
        "control", "or", "conf.int", "conf.level", "simulate.p.value", "B"), formals = formals(
        stats::fisher.test), passed_args = list(), lst_ard_columns = list(context = "fishertest",
        group1 = "am", variable = "vs")))
    Output
         group1 variable    context        stat_name
      1      am       vs fishertest         estimate
      2      am       vs fishertest          p.value
      3      am       vs fishertest           method
      4      am       vs fishertest        workspace
      5      am       vs fishertest           hybrid
      6      am       vs fishertest       hybridPars
      7      am       vs fishertest          control
      8      am       vs fishertest               or
      9      am       vs fishertest         conf.int
      10     am       vs fishertest       conf.level
      11     am       vs fishertest simulate.p.value
      12     am       vs fishertest                B
                                          stat fmt_fn warning error
      1                               1.956055      1    NULL  NULL
      2                              0.4726974      1    NULL  NULL
      3     Fisher's Exact Test for Count Data   NULL    NULL  NULL
      4                                  2e+05      1    NULL  NULL
      5                                  FALSE   NULL    NULL  NULL
      6  c(expect = 5, percent = 80, Emin = 1)   NULL    NULL  NULL
      7                                 list()   NULL    NULL  NULL
      8                                      1      1    NULL  NULL
      9                                   TRUE   NULL    NULL  NULL
      10                                  0.95      1    NULL  NULL
      11                                 FALSE   NULL    NULL  NULL
      12                                  2000      1    NULL  NULL

---

    Code
      as.data.frame(tidy_as_ard(lst_tidy = eval_capture_conditions(stop(
        "Planned unit testing error!")), tidy_result_names = c("estimate", "p.value",
        "conf.low", "conf.high", "method", "alternative"), fun_args_to_record = c(
        "workspace", "hybrid", "hybridPars", "control", "or", "conf.int",
        "conf.level", "simulate.p.value", "B"), formals = formals(stats::fisher.test),
      passed_args = list(), lst_ard_columns = list(context = "fishertest", group1 = "am",
        variable = "vs")))
    Output
         group1 variable    context        stat_name
      1      am       vs fishertest         estimate
      2      am       vs fishertest          p.value
      3      am       vs fishertest         conf.low
      4      am       vs fishertest        conf.high
      5      am       vs fishertest           method
      6      am       vs fishertest      alternative
      7      am       vs fishertest        workspace
      8      am       vs fishertest           hybrid
      9      am       vs fishertest       hybridPars
      10     am       vs fishertest          control
      11     am       vs fishertest               or
      12     am       vs fishertest         conf.int
      13     am       vs fishertest       conf.level
      14     am       vs fishertest simulate.p.value
      15     am       vs fishertest                B
                                          stat fmt_fn warning
      1                                   NULL   NULL    NULL
      2                                   NULL   NULL    NULL
      3                                   NULL   NULL    NULL
      4                                   NULL   NULL    NULL
      5                                   NULL   NULL    NULL
      6                                   NULL   NULL    NULL
      7                                  2e+05      1    NULL
      8                                  FALSE   NULL    NULL
      9  c(expect = 5, percent = 80, Emin = 1)   NULL    NULL
      10                                list()   NULL    NULL
      11                                     1      1    NULL
      12                                  TRUE   NULL    NULL
      13                                  0.95      1    NULL
      14                                 FALSE   NULL    NULL
      15                                  2000      1    NULL
                               error
      1  Planned unit testing error!
      2  Planned unit testing error!
      3  Planned unit testing error!
      4  Planned unit testing error!
      5  Planned unit testing error!
      6  Planned unit testing error!
      7  Planned unit testing error!
      8  Planned unit testing error!
      9  Planned unit testing error!
      10 Planned unit testing error!
      11 Planned unit testing error!
      12 Planned unit testing error!
      13 Planned unit testing error!
      14 Planned unit testing error!
      15 Planned unit testing error!

---

    Code
      dplyr::select(as.data.frame(tidy_as_ard(lst_tidy = eval_capture_conditions(
        dplyr::as_tibble(stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c(
          "estimate", "p.value", "method")])), tidy_result_names = c("estimate",
        "p.value", "conf.low", "conf.high", "method", "alternative"), formals = formals(
        stats::fisher.test), passed_args = list(), lst_ard_columns = list(context = "fishertest",
        group1 = "am", variable = "vs"))), c(group1, variable, stat))
    Output
        group1 variable                               stat
      1     am       vs                           1.956055
      2     am       vs                          0.4726974
      3     am       vs Fisher's Exact Test for Count Data

---

    Code
      dplyr::select(as.data.frame(tidy_as_ard(lst_tidy = eval_capture_conditions(
        dplyr::as_tibble(stats::fisher.test(x = mtcars[["am"]], y = mtcars[["vs"]])[c(
          "estimate", "p.value", "method")])), tidy_result_names = c("estimate",
        "p.value", "conf.low", "conf.high", "method", "alternative"), passed_args = list(),
      lst_ard_columns = list(context = "fishertest", group1 = "am", variable = "vs"))),
      c(group1, variable, stat))
    Output
        group1 variable                               stat
      1     am       vs                           1.956055
      2     am       vs                          0.4726974
      3     am       vs Fisher's Exact Test for Count Data

