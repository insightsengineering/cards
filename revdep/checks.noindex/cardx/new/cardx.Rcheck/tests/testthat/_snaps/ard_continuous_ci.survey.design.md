# ard_continuous_ci(data)

    Code
      dplyr::select(as.data.frame(ard_continuous_ci(dclus1, variables = c(api00,
        api99))), -warning, -error)
    Output
         variable              context  stat_name stat_label     stat fmt_fn
      1     api00 survey_continuous_ci   estimate   estimate 644.1694      2
      2     api00 survey_continuous_ci  std.error  std.error 23.54224      2
      3     api00 survey_continuous_ci   conf.low   conf.low 593.6763      2
      4     api00 survey_continuous_ci  conf.high  conf.high 694.6625      2
      5     api00 survey_continuous_ci conf.level conf.level     0.95      2
      6     api99 survey_continuous_ci   estimate   estimate 606.9781      2
      7     api99 survey_continuous_ci  std.error  std.error 24.22504      2
      8     api99 survey_continuous_ci   conf.low   conf.low 555.0206      2
      9     api99 survey_continuous_ci  conf.high  conf.high 658.9357      2
      10    api99 survey_continuous_ci conf.level conf.level     0.95      2

# ard_continuous_ci() errors are captured

    Code
      ard_continuous_ci(dclus1, variables = c(api00, api99), df = letters)
    Message
      {cards} data frame: 10 x 8
    Output
         variable   context  stat_name stat_label stat     error
      1     api00 survey_c…   estimate   estimate      Non-nume…
      2     api00 survey_c…  std.error  std.error      Non-nume…
      3     api00 survey_c…   conf.low   conf.low      Non-nume…
      4     api00 survey_c…  conf.high  conf.high      Non-nume…
      5     api00 survey_c… conf.level  conf.lev… 0.95 Non-nume…
      6     api99 survey_c…   estimate   estimate      Non-nume…
      7     api99 survey_c…  std.error  std.error      Non-nume…
      8     api99 survey_c…   conf.low   conf.low      Non-nume…
      9     api99 survey_c…  conf.high  conf.high      Non-nume…
      10    api99 survey_c… conf.level  conf.lev… 0.95 Non-nume…
    Message
      i 2 more variables: fmt_fn, warning

---

    Code
      ard_continuous_ci(dclus1, variables = sch.wide, method = "svymedian.beta")
    Message
      Column "sch.wide" is not <numeric> and results may be an unexpected format.
      {cards} data frame: 5 x 8
    Output
        variable  stat_name stat_label stat   warning     error
      1 sch.wide   estimate   estimate      '<=' not… error in…
      2 sch.wide  std.error  std.error      '<=' not… error in…
      3 sch.wide   conf.low   conf.low      '<=' not… error in…
      4 sch.wide  conf.high  conf.high      '<=' not… error in…
      5 sch.wide conf.level  conf.lev… 0.95 '<=' not… error in…
    Message
      i 2 more variables: context, fmt_fn

