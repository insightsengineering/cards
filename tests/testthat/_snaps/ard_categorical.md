# ard_continuous(fmt_fn) argument works

    Code
      as.data.frame(dplyr::select(apply_statistic_fmt_fn(ard_categorical(mtcars,
        variables = "am", fmt_fn = list(am = list(p = function(x) as.character(round(
          x * 100, digits = 3)), N = function(x) format(round(x, digits = 2), nsmall = 2),
        N_obs = function(x) format(round(x, digits = 2), nsmall = 2))))), variable,
      variable_level, stat_name, statistic, statistic_fmt))
    Output
        variable variable_level stat_name statistic statistic_fmt
      1       am              0         n        19            19
      2       am              0         p   0.59375        59.375
      3       am              1         n        13            13
      4       am              1         p   0.40625        40.625
      5       am           NULL         N        32         32.00

# ard_categorical(stat_labels) argument works

    Code
      unique(dplyr::select(dplyr::filter(ard_categorical(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_labels = everything() ~ list(c("n", "p") ~
          "n (pct)")), stat_name %in% c("n", "p")), stat_name, stat_label))
    Output
      # A tibble: 2 x 2
        stat_name stat_label
        <chr>     <chr>     
      1 n         n (pct)   
      2 p         n (pct)   

---

    Code
      unique(dplyr::select(dplyr::filter(ard_categorical(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_labels = everything() ~ list(n = "num",
          p = "pct")), stat_name %in% c("n", "p")), stat_name, stat_label))
    Output
      # A tibble: 2 x 2
        stat_name stat_label
        <chr>     <chr>     
      1 n         num       
      2 p         pct       

---

    Code
      unique(dplyr::select(dplyr::filter(ard_categorical(data = ADSL, by = "ARM",
        variables = c("AGEGR1", "SEX"), stat_labels = AGEGR1 ~ list(c("n", "p") ~
          "n (pct)")), stat_name %in% c("n", "p")), variable, stat_name, stat_label))
    Output
      # A tibble: 4 x 3
        variable stat_name stat_label
        <chr>    <chr>     <chr>     
      1 AGEGR1   n         n (pct)   
      2 AGEGR1   p         n (pct)   
      3 SEX      n         n         
      4 SEX      p         %         

# ard_categorical() messaging

    Code
      ard_categorical(mtcars, by = cyl, variables = am, denominator = iris)
    Condition
      Error in `ard_categorical()`:
      ! Columns "cyl" must appear in `denominator`.

