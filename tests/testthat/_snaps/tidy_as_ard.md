# tidy_as_ard() works

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

