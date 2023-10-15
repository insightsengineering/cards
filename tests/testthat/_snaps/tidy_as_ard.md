# tidy_as_ard() works

    Code
      as.data.frame(tidy_as_ard(lst_tidy = eval_capture_conditions(broom::tidy(stats::fisher.test(
        x = mtcars[["am"]], y = mtcars[["vs"]]))), tidy_result_names = c("estimate",
        "p.value", "conf.low", "conf.high", "method", "alternative"),
      fun_args_to_record = c("workspace", "hybrid", "hybridPars", "control", "or",
        "conf.int", "conf.level", "simulate.p.value", "B"), formals = formals(stats::fisher.test),
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
                                     statistic warning error
      1                               1.956055    NULL  NULL
      2                              0.4726974    NULL  NULL
      3                              0.3825342    NULL  NULL
      4                               10.59161    NULL  NULL
      5     Fisher's Exact Test for Count Data    NULL  NULL
      6                              two.sided    NULL  NULL
      7                                  2e+05    NULL  NULL
      8                                  FALSE    NULL  NULL
      9  c(expect = 5, percent = 80, Emin = 1)    NULL  NULL
      10                                list()    NULL  NULL
      11                                     1    NULL  NULL
      12                                  TRUE    NULL  NULL
      13                                  0.95    NULL  NULL
      14                                 FALSE    NULL  NULL
      15                                  2000    NULL  NULL

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
                                     statistic warning                       error
      1                                   NULL    NULL Planned unit testing error!
      2                                   NULL    NULL Planned unit testing error!
      3                                   NULL    NULL Planned unit testing error!
      4                                   NULL    NULL Planned unit testing error!
      5                                   NULL    NULL Planned unit testing error!
      6                                   NULL    NULL Planned unit testing error!
      7                                  2e+05    NULL Planned unit testing error!
      8                                  FALSE    NULL Planned unit testing error!
      9  c(expect = 5, percent = 80, Emin = 1)    NULL Planned unit testing error!
      10                                list()    NULL Planned unit testing error!
      11                                     1    NULL Planned unit testing error!
      12                                  TRUE    NULL Planned unit testing error!
      13                                  0.95    NULL Planned unit testing error!
      14                                 FALSE    NULL Planned unit testing error!
      15                                  2000    NULL Planned unit testing error!

