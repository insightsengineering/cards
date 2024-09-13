pkgname <- "gtsummary"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('gtsummary')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_ci")
### * add_ci

flush(stderr()); flush(stdout())

### Name: add_ci
### Title: Add CI Column
### Aliases: add_ci add_ci.tbl_summary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  tbl_summary(
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd})",
    include = c(marker, response, trt)
  ) |>
  add_ci()

# Example 2 ----------------------------------
trial |>
  select(response, grade) %>%
  tbl_summary(
    statistic = all_categorical() ~ "{p}%",
    missing = "no",
    include = c(response, grade)
  ) |>
  add_ci(pattern = "{stat} ({ci})") |>
  modify_footnote(everything() ~ NA)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_ci.tbl_svysummary")
### * add_ci.tbl_svysummary

flush(stderr()); flush(stdout())

### Name: add_ci.tbl_svysummary
### Title: Add CI Column
### Aliases: add_ci.tbl_svysummary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "survey"), reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
  tbl_svysummary(
    by = "both",
    include = c(api00, stype),
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) |>
  add_stat_label() |>
  add_ci(pattern = "{stat} (95% CI {ci})") |>
  modify_header(all_stat_cols() ~ "**{level}**") |>
  modify_spanning_header(all_stat_cols() ~ "**Survived**")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_difference.tbl_summary")
### * add_difference.tbl_summary

flush(stderr()); flush(stdout())

### Name: add_difference.tbl_summary
### Title: Add differences between groups
### Aliases: add_difference.tbl_summary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  select(trt, age, marker, response, death) %>%
  tbl_summary(
    by = trt,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{p}%"
      ),
    missing = "no"
  ) |>
  add_n() |>
  add_difference()

# Example 2 ----------------------------------
# ANCOVA adjusted for grade and stage
trial |>
  select(trt, age, marker, grade, stage) %>%
  tbl_summary(
    by = trt,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no",
    include = c(age, marker, trt)
  ) |>
  add_n() |>
  add_difference(adj.vars = c(grade, stage))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_difference.tbl_svysummary")
### * add_difference.tbl_svysummary

flush(stderr()); flush(stdout())

### Name: add_difference.tbl_svysummary
### Title: Add differences between groups
### Aliases: add_difference.tbl_svysummary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_glance")
### * add_glance

flush(stderr()); flush(stdout())

### Name: add_glance
### Title: Add model statistics
### Aliases: add_glance add_glance_table add_glance_source_note

### ** Examples

mod <- lm(age ~ marker + grade, trial) |> tbl_regression()

# Example 1 ----------------------------------
mod |>
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )

# Example 2 ----------------------------------
mod |>
  add_glance_source_note(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )



cleanEx()
nameEx("add_global_p")
### * add_global_p

flush(stderr()); flush(stdout())

### Name: add_global_p
### Title: Add the global p-values
### Aliases: add_global_p add_global_p.tbl_regression
###   add_global_p.tbl_uvregression

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "broom", "car"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
lm(marker ~ age + grade, trial) |>
  tbl_regression() |>
  add_global_p()

# Example 2 ----------------------------------
trial[c("response", "age", "trt", "grade")] |>
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |>
  add_global_p()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_n.tbl_survfit")
### * add_n.tbl_survfit

flush(stderr()); flush(stdout())

### Name: add_n.tbl_survfit
### Title: Add N
### Aliases: add_n.tbl_survfit

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("survival", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)
fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)

# Example 1 ----------------------------------
list(fit1, fit2) |>
  tbl_survfit(times = c(12, 24)) |>
  add_n()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_n_regression")
### * add_n_regression

flush(stderr()); flush(stdout())

### Name: add_n_regression
### Title: Add N to regression table
### Aliases: add_n_regression add_n.tbl_regression add_n.tbl_uvregression

### ** Examples

# Example 1 ----------------------------------
trial |>
  select(response, age, grade) |>
  tbl_uvregression(
    y = response,
    exponentiate = TRUE,
    method = glm,
    method.args = list(family = binomial),
    hide_n = TRUE
  ) |>
  add_n(location = "label")

# Example 2 ----------------------------------
glm(response ~ age + grade, trial, family = binomial) |>
  tbl_regression(exponentiate = TRUE) |>
  add_n(location = "level")



cleanEx()
nameEx("add_n_summary")
### * add_n_summary

flush(stderr()); flush(stdout())

### Name: add_n_summary
### Title: Add column with N
### Aliases: add_n_summary add_n.tbl_summary add_n.tbl_svysummary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("survey", "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
  add_n()

# Example 2 ----------------------------------
survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
  tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age)) |>
  add_n()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_nevent.tbl_survfit")
### * add_nevent.tbl_survfit

flush(stderr()); flush(stdout())

### Name: add_nevent.tbl_survfit
### Title: Add event N
### Aliases: add_nevent.tbl_survfit

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("survival", "broom"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)
fit1 <- survfit(Surv(ttdeath, death) ~ 1, trial)
fit2 <- survfit(Surv(ttdeath, death) ~ trt, trial)

# Example 1 ----------------------------------
list(fit1, fit2) |>
  tbl_survfit(times = c(12, 24)) |>
  add_n() |>
  add_nevent()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_nevent_regression")
### * add_nevent_regression

flush(stderr()); flush(stdout())

### Name: add_nevent_regression
### Title: Add event N
### Aliases: add_nevent_regression add_nevent add_nevent.tbl_regression
###   add_nevent.tbl_uvregression

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("broom.helpers", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  select(response, trt, grade) |>
  tbl_uvregression(
    y = response,
    exponentiate = TRUE,
    method = glm,
    method.args = list(family = binomial),
  ) |>
  add_nevent()

# Example 2 ----------------------------------
glm(response ~ age + grade, trial, family = binomial) |>
  tbl_regression(exponentiate = TRUE) |>
  add_nevent(location = "level")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_overall")
### * add_overall

flush(stderr()); flush(stdout())

### Name: add_overall
### Title: Add overall column
### Aliases: add_overall add_overall.tbl_summary add_overall.tbl_continuous
###   add_overall.tbl_svysummary add_overall.tbl_custom_summary

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_summary(include = c(age, grade), by = trt) |>
  add_overall()

# Example 2 ----------------------------------
trial |>
  tbl_summary(
    include = grade,
    by = trt,
    percent = "row",
    statistic = ~"{p}%",
    digits = ~1
  ) |>
  add_overall(
    last = TRUE,
    statistic = ~"{p}% (n={n})",
    digits = ~ c(1, 0)
  )

# Example 3 ----------------------------------
trial |>
  tbl_continuous(
    variable = age,
    by = trt,
    include = grade
  ) |>
  add_overall(last = TRUE)



cleanEx()
nameEx("add_p.tbl_continuous")
### * add_p.tbl_continuous

flush(stderr()); flush(stdout())

### Name: add_p.tbl_continuous
### Title: Add p-values
### Aliases: add_p.tbl_continuous

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
trial |>
  tbl_continuous(variable = age, by = trt, include = grade) |>
  add_p()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_p.tbl_cross")
### * add_p.tbl_cross

flush(stderr()); flush(stdout())

### Name: add_p.tbl_cross
### Title: Add p-value
### Aliases: add_p.tbl_cross

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_cross(row = stage, col = trt) |>
  add_p()

# Example 2 ----------------------------------
trial |>
  tbl_cross(row = stage, col = trt) |>
  add_p(source_note = TRUE)



cleanEx()
nameEx("add_p.tbl_summary")
### * add_p.tbl_summary

flush(stderr()); flush(stdout())

### Name: add_p.tbl_summary
### Title: Add p-values
### Aliases: add_p.tbl_summary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p()

# Example 2 ----------------------------------
trial |>
  select(trt, age, marker) |>
  tbl_summary(by = trt, missing = "no") |>
  add_p(
    # perform t-test for all variables
    test = everything() ~ "t.test",
    # assume equal variance in the t-test
    test.args = all_tests("t.test") ~ list(var.equal = TRUE)
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_p.tbl_survfit")
### * add_p.tbl_survfit

flush(stderr()); flush(stdout())

### Name: add_p.tbl_survfit
### Title: Add p-value
### Aliases: add_p.tbl_survfit

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("survival", "broom"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)

gts_survfit <-
  list(
    survfit(Surv(ttdeath, death) ~ grade, trial),
    survfit(Surv(ttdeath, death) ~ trt, trial)
  ) |>
  tbl_survfit(times = c(12, 24))

# Example 1 ----------------------------------
gts_survfit |>
  add_p()

# Example 2 ----------------------------------
# Pass `rho=` argument to `survdiff()`
gts_survfit |>
  add_p(test = "survdiff", test.args = list(rho = 0.5))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_p.tbl_svysummary")
### * add_p.tbl_svysummary

flush(stderr()); flush(stdout())

### Name: add_p.tbl_svysummary
### Title: Add p-values
### Aliases: add_p.tbl_svysummary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "survey"), reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
# A simple weighted dataset
survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
  tbl_svysummary(by = Survived, include = c(Sex, Age)) |>
  add_p()

# A dataset with a complex design
data(api, package = "survey")
d_clust <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

# Example 2 ----------------------------------
tbl_svysummary(d_clust, by = both, include = c(api00, api99)) |>
  add_p()

# Example 3 ----------------------------------
# change tests to svy t-test and Wald test
tbl_svysummary(d_clust, by = both, include = c(api00, api99, stype)) |>
  add_p(
    test = list(
      all_continuous() ~ "svy.t.test",
      all_categorical() ~ "svy.wald.test"
    )
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_q")
### * add_q

flush(stderr()); flush(stdout())

### Name: add_q
### Title: Add multiple comparison adjustment
### Aliases: add_q

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
add_q_ex1 <-
  trial |>
  tbl_summary(by = trt, include = c(trt, age, grade, response)) |>
  add_p() |>
  add_q()

# Example 2 ----------------------------------
trial |>
  tbl_uvregression(
    y = response,
    include = c("trt", "age", "grade"),
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE
  ) |>
  add_global_p() |>
  add_q()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_significance_stars")
### * add_significance_stars

flush(stderr()); flush(stdout())

### Name: add_significance_stars
### Title: Add significance stars
### Aliases: add_significance_stars

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("car", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
tbl <-
  lm(time ~ ph.ecog + sex, survival::lung) |>
  tbl_regression(label = list(ph.ecog = "ECOG Score", sex = "Sex"))

# Example 1 ----------------------------------
tbl |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)

# Example 2 ----------------------------------
tbl |>
  add_significance_stars(
    pattern = "{estimate} ({conf.low}, {conf.high}){stars}",
    hide_ci = TRUE, hide_se = TRUE
  ) |>
  modify_header(estimate = "**Beta (95% CI)**") |>
  modify_footnote(estimate = "CI = Confidence Interval", abbreviation = TRUE)

# Example 3 ----------------------------------
# Use '  \n' to put a line break between beta and SE
tbl |>
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}  \n({std.error})"
  ) |>
  modify_header(estimate = "**Beta  \n(SE)**") |>
  modify_footnote(estimate = "SE = Standard Error", abbreviation = TRUE) |>
  as_gt() |>
  gt::fmt_markdown(columns = everything()) |>
  gt::tab_style(
    style = "vertical-align:top",
    locations = gt::cells_body(columns = label)
  )

# Example 4 ----------------------------------
lm(marker ~ stage + grade, data = trial) |>
  tbl_regression() |>
  add_global_p() |>
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}"
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("add_stat")
### * add_stat

flush(stderr()); flush(stdout())

### Name: add_stat
### Title: Add a custom statistic
### Aliases: add_stat

### ** Examples

# Example 1 ----------------------------------
# fn returns t-test pvalue
my_ttest <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
}

trial |>
  tbl_summary(
    by = trt,
    include = c(trt, age, marker),
    missing = "no"
  ) |>
  add_stat(fns = everything() ~ my_ttest) |>
  modify_header(add_stat_1 = "**p-value**", all_stat_cols() ~ "**{level}**")

# Example 2 ----------------------------------
# fn returns t-test test statistic and pvalue
my_ttest2 <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]])) |>
    broom::tidy() %>%
    dplyr::mutate(
      stat = glue::glue("t={style_sigfig(statistic)}, {style_pvalue(p.value, prepend_p = TRUE)}")
    ) %>%
    dplyr::pull(stat)
}

trial |>
  tbl_summary(
    by = trt,
    include = c(trt, age, marker),
    missing = "no"
  ) |>
  add_stat(fns = everything() ~ my_ttest2) |>
  modify_header(add_stat_1 = "**Treatment Comparison**")

# Example 3 ----------------------------------
# return test statistic and p-value is separate columns
my_ttest3 <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
    broom::tidy() %>%
    select(statistic, p.value)
}

trial |>
  tbl_summary(
    by = trt,
    include = c(trt, age, marker),
    missing = "no"
  ) |>
  add_stat(fns = everything() ~ my_ttest3) |>
  modify_header(statistic = "**t-statistic**", p.value = "**p-value**") |>
  modify_fmt_fun(statistic = label_style_sigfig(), p.value = label_style_pvalue(digits = 2))



cleanEx()
nameEx("add_stat_label")
### * add_stat_label

flush(stderr()); flush(stdout())

### Name: add_stat_label
### Title: Add statistic labels
### Aliases: add_stat_label add_stat_label.tbl_summary
###   add_stat_label.tbl_svysummary

### ** Examples

tbl <- trial |>
  dplyr::select(trt, age, grade, response) |>
  tbl_summary(by = trt)

# Example 1 ----------------------------------
# Add statistic presented to the variable label row
tbl |>
  add_stat_label(
    # update default statistic label for continuous variables
    label = all_continuous() ~ "med. (iqr)"
  )

# Example 2 ----------------------------------
tbl |>
  add_stat_label(
    # add a new column with statistic labels
    location = "column"
  )

# Example 3 ----------------------------------
trial |>
  select(age, grade, trt) |>
  tbl_summary(
    by = trt,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min} - {max}"),
  ) |>
  add_stat_label(label = age ~ c("IQR", "Range"))



cleanEx()
nameEx("add_vif")
### * add_vif

flush(stderr()); flush(stdout())

### Name: add_vif
### Title: Add Variance Inflation Factor
### Aliases: add_vif

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "car"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
lm(age ~ grade + marker, trial) |>
  tbl_regression() |>
  add_vif()

# Example 2 ----------------------------------
lm(age ~ grade + marker, trial) |>
  tbl_regression() |>
  add_vif(c("aGVIF", "df"))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("as_flex_table")
### * as_flex_table

flush(stderr()); flush(stdout())

### Name: as_flex_table
### Title: Convert gtsummary object to a flextable object
### Aliases: as_flex_table

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("flextable", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
trial |>
  select(trt, age, grade) |>
  tbl_summary(by = trt) |>
  add_p() |>
  as_flex_table()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("as_gt")
### * as_gt

flush(stderr()); flush(stdout())

### Name: as_gt
### Title: Convert gtsummary object to gt
### Aliases: as_gt

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade, response)) |>
  as_gt()



cleanEx()
nameEx("as_hux_table")
### * as_hux_table

flush(stderr()); flush(stdout())

### Name: as_hux_table
### Title: Convert gtsummary object to a huxtable object
### Aliases: as_hux_table as_hux_xlsx

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("huxtable", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  as_hux_table()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("as_kable")
### * as_kable

flush(stderr()); flush(stdout())

### Name: as_kable
### Title: Convert gtsummary object to a kable object
### Aliases: as_kable

### ** Examples

trial |>
  tbl_summary(by = trt) |>
  bold_labels() |>
  as_kable()



cleanEx()
nameEx("as_kable_extra")
### * as_kable_extra

flush(stderr()); flush(stdout())

### Name: as_kable_extra
### Title: Convert gtsummary object to a kableExtra object
### Aliases: as_kable_extra

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("kableExtra", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# basic gtsummary tbl to build upon
as_kable_extra_base <-
  trial |>
  tbl_summary(by = trt, include = c(age, stage)) |>
  bold_labels()

# Example 1 (PDF via LaTeX) ---------------------
# add linebreak in table header with '\n'
as_kable_extra_ex1_pdf <-
  as_kable_extra_base |>
  modify_header(all_stat_cols() ~ "**{level}**  \n*N = {n}*") |>
  as_kable_extra()

# Example 2 (PDF via LaTeX) ---------------------
# additional styling in `knitr::kable()` and with
#   call to `kableExtra::kable_styling()`
as_kable_extra_ex2_pdf <-
  as_kable_extra_base |>
  as_kable_extra(
    booktabs = TRUE,
    longtable = TRUE,
    linesep = ""
  ) |>
  kableExtra::kable_styling(
    position = "left",
    latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15"
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("as_tibble.gtsummary")
### * as_tibble.gtsummary

flush(stderr()); flush(stdout())

### Name: as_tibble.gtsummary
### Title: Convert gtsummary object to a tibble
### Aliases: as_tibble.gtsummary as.data.frame.gtsummary

### ** Examples

tbl <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade, response))

as_tibble(tbl)

# without column labels
as_tibble(tbl, col_labels = FALSE)



cleanEx()
nameEx("assign_summary_digits")
### * assign_summary_digits

flush(stderr()); flush(stdout())

### Name: assign_summary_digits
### Title: Assign Default Digits
### Aliases: assign_summary_digits

### ** Examples

assign_summary_digits(
  mtcars,
  statistic = list(mpg = "{mean}"),
  type = list(mpg = "continuous")
)



cleanEx()
nameEx("assign_summary_type")
### * assign_summary_type

flush(stderr()); flush(stdout())

### Name: assign_summary_type
### Title: Assign Default Summary Type
### Aliases: assign_summary_type

### ** Examples

assign_summary_type(
  data = trial,
  variables = c("age", "grade", "response"),
  value = NULL
)



cleanEx()
nameEx("assign_tests")
### * assign_tests

flush(stderr()); flush(stdout())

### Name: assign_tests
### Title: Assign Test
### Aliases: assign_tests assign_tests.tbl_summary
###   assign_tests.tbl_svysummary assign_tests.tbl_continuous
###   assign_tests.tbl_survfit

### ** Examples

trial |>
  tbl_summary(
    by = trt,
    include = c(age, stage)
  ) |>
  assign_tests(include = c("age", "stage"), calling_fun = "add_p")



cleanEx()
nameEx("bold_italicize_labels_levels")
### * bold_italicize_labels_levels

flush(stderr()); flush(stdout())

### Name: bold_italicize_labels_levels
### Title: Bold or Italicize
### Aliases: bold_italicize_labels_levels bold_labels italicize_labels
###   bold_levels italicize_levels bold_labels.gtsummary
###   bold_levels.gtsummary italicize_labels.gtsummary
###   italicize_levels.gtsummary bold_labels.tbl_cross
###   bold_levels.tbl_cross italicize_labels.tbl_cross
###   italicize_levels.tbl_cross

### ** Examples

# Example 1 ----------------------------------
tbl_summary(trial, include = c("trt", "age", "response")) |>
  bold_labels() |>
  bold_levels() |>
  italicize_labels() |>
  italicize_levels()



cleanEx()
nameEx("bold_p")
### * bold_p

flush(stderr()); flush(stdout())

### Name: bold_p
### Title: Bold significant p-values
### Aliases: bold_p

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
  add_p() |>
  bold_p(t = 0.1)

# Example 2 ----------------------------------
glm(response ~ trt + grade, trial, family = binomial(link = "logit")) |>
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.65)



cleanEx()
nameEx("brdg_continuous")
### * brdg_continuous

flush(stderr()); flush(stdout())

### Name: brdg_continuous
### Title: Continuous Summary Table Bridges
### Aliases: brdg_continuous

### ** Examples

library(cards)

bind_ard(
  # the primary ARD with the results
  ard_continuous(trial, by = grade, variables = age),
  # add missing and attributes ARD
  ard_missing(trial, by = grade, variables = age),
  ard_attributes(trial, variables = c(grade, age))
) |>
  # adding the column name
  dplyr::mutate(
    gts_column =
      ifelse(!context %in% "attributes", "stat_0", NA_character_)
  ) |>
  brdg_continuous(
    variable = "age",
    include = "grade",
    statistic = list(grade = "{median} ({p25}, {p75})")
 ) |>
 as_tibble()



cleanEx()
nameEx("brdg_summary")
### * brdg_summary

flush(stderr()); flush(stdout())

### Name: brdg_summary
### Title: Summary table bridge
### Aliases: brdg_summary pier_summary_dichotomous pier_summary_categorical
###   pier_summary_continuous2 pier_summary_continuous
###   pier_summary_missing_row

### ** Examples

library(cards)

# first build ARD data frame
cards <-
  ard_stack(
    mtcars,
    ard_continuous(variables = c("mpg", "hp")),
    ard_categorical(variables = "cyl"),
    ard_dichotomous(variables = "am"),
    .missing = TRUE,
    .attributes = TRUE
  ) |>
  # this column is used by the `pier_*()` functions
  dplyr::mutate(gts_column = ifelse(context == "attributes", NA, "stat_0"))

brdg_summary(
  cards = cards,
  variables = c("cyl", "am", "mpg", "hp"),
  type =
    list(
      cyl = "categorical",
      am = "dichotomous",
      mpg = "continuous",
      hp = "continuous2"
    ),
  statistic =
    list(
      cyl = "{n} / {N}",
      am = "{n} / {N}",
      mpg = "{mean} ({sd})",
      hp = c("{median} ({p25}, {p75})", "{mean} ({sd})")
    )
) |>
  as_tibble()

pier_summary_dichotomous(
  cards = cards,
  variables = "am",
  statistic = list(am = "{n} ({p})")
)

pier_summary_categorical(
  cards = cards,
  variables = "cyl",
  statistic = list(cyl = "{n} ({p})")
)

pier_summary_continuous2(
  cards = cards,
  variables = "hp",
  statistic = list(hp = c("{median}", "{mean}"))
)

pier_summary_continuous(
  cards = cards,
  variables = "mpg",
  statistic = list(mpg = "{median}")
)



cleanEx()
nameEx("brdg_wide_summary")
### * brdg_wide_summary

flush(stderr()); flush(stdout())

### Name: brdg_wide_summary
### Title: Wide summary table bridge
### Aliases: brdg_wide_summary

### ** Examples

library(cards)

bind_ard(
  ard_continuous(trial, variables = c(age, marker)),
  ard_attributes(trial, variables = c(age, marker))
) |>
  brdg_wide_summary(
    variables = c("age", "marker"),
    statistic = list(age = c("{mean}", "{sd}"), marker = c("{mean}", "{sd}")),
    type = list(age = "continuous", marker = "continuous")
  )



cleanEx()
nameEx("combine_terms")
### * combine_terms

flush(stderr()); flush(stdout())

### Name: combine_terms
### Title: Combine terms
### Aliases: combine_terms

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c('cardx', 'broom.helpers'))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
# Logistic Regression Example, LRT p-value
glm(response ~ marker + I(marker^2) + grade,
    trial[c("response", "marker", "grade")] |> na.omit(), # keep complete cases only!
    family = binomial) |>
  tbl_regression(label = grade ~ "Grade", exponentiate = TRUE) |>
  # collapse non-linear terms to a single row in output using anova
  combine_terms(
    formula_update = . ~ . - marker - I(marker^2),
    label = "Marker (non-linear terms)",
    test = "LRT"
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("custom_tidiers")
### * custom_tidiers

flush(stderr()); flush(stdout())

### Name: custom_tidiers
### Title: Custom tidiers
### Aliases: custom_tidiers tidy_standardize tidy_bootstrap tidy_robust
###   pool_and_tidy_mice tidy_gam tidy_wald_test

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("effectsize", "mice", "parameters"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
mod <- lm(age ~ marker + grade, trial)

tbl_stnd <- tbl_regression(mod, tidy_fun = tidy_standardize)
tbl <- tbl_regression(mod)

tidy_standardize_ex1 <-
  tbl_merge(
    list(tbl_stnd, tbl),
    tab_spanner = c("**Standardized Model**", "**Original Model**")
  )

# Example 2 ----------------------------------
# use "posthoc" method for coef calculation
tbl_regression(mod, tidy_fun = \(x, ...) tidy_standardize(x, method = "posthoc", ...))

# Example 3 ----------------------------------
# Multiple Imputation using the mice package
set.seed(1123)
pool_and_tidy_mice_ex3 <-
  suppressWarnings(mice::mice(trial, m = 2)) |>
  with(lm(age ~ marker + grade)) |>
  tbl_regression()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-list2tb")
### * dot-list2tb

flush(stderr()); flush(stdout())

### Name: .list2tb
### Title: Convert Named List to Table Body
### Aliases: .list2tb
### Keywords: internal

### ** Examples

type <- list(age = "continuous", response = "dichotomous")
gtsummary:::.list2tb(type, "var_type")



cleanEx()
nameEx("dot-table_styling_expr_to_row_number")
### * dot-table_styling_expr_to_row_number

flush(stderr()); flush(stdout())

### Name: .table_styling_expr_to_row_number
### Title: Object Convert Helper
### Aliases: .table_styling_expr_to_row_number
### Keywords: internal

### ** Examples

tbl <-
  trial %>%
  tbl_summary(include = c(age, grade)) %>%
  .table_styling_expr_to_row_number()



cleanEx()
nameEx("global_pvalue_fun")
### * global_pvalue_fun

flush(stderr()); flush(stdout())

### Name: global_pvalue_fun
### Title: Global p-value generic
### Aliases: global_pvalue_fun global_pvalue_fun.default
###   global_pvalue_fun.geeglm global_pvalue_fun.tidycrr
### Keywords: internal

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "broom", "car"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
lm(age ~ stage + grade, trial) |>
  global_pvalue_fun(type = "III")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("inline_text.tbl_continuous")
### * inline_text.tbl_continuous

flush(stderr()); flush(stdout())

### Name: inline_text.tbl_continuous
### Title: Report statistics from summary tables inline
### Aliases: inline_text.tbl_continuous

### ** Examples

t1 <- trial |>
  tbl_summary(by = trt, include = grade) |>
  add_p()

inline_text(t1, variable = grade, level = "I", column = "Drug A", pattern = "{n}/{N} ({p}%)")
inline_text(t1, variable = grade, column = "p.value")



cleanEx()
nameEx("inline_text.tbl_cross")
### * inline_text.tbl_cross

flush(stderr()); flush(stdout())

### Name: inline_text.tbl_cross
### Title: Report statistics from cross table inline
### Aliases: inline_text.tbl_cross

### ** Examples

tbl_cross <-
  tbl_cross(trial, row = trt, col = response) %>%
  add_p()

inline_text(tbl_cross, row_level = "Drug A", col_level = "1")
inline_text(tbl_cross, row_level = "Total", col_level = "1")
inline_text(tbl_cross, col_level = "p.value")



cleanEx()
nameEx("inline_text.tbl_regression")
### * inline_text.tbl_regression

flush(stderr()); flush(stdout())

### Name: inline_text.tbl_regression
### Title: Report statistics from regression summary tables inline
### Aliases: inline_text.tbl_regression

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
inline_text_ex1 <-
  glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE)

inline_text(inline_text_ex1, variable = age)
inline_text(inline_text_ex1, variable = grade, level = "III")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("inline_text.tbl_summary")
### * inline_text.tbl_summary

flush(stderr()); flush(stdout())

### Name: inline_text.tbl_summary
### Title: Report statistics from summary tables inline
### Aliases: inline_text.tbl_summary inline_text.tbl_svysummary

### ** Examples

t1 <- trial |>
  tbl_summary(by = trt, include = grade) |>
  add_p()

inline_text(t1, variable = grade, level = "I", column = "Drug A", pattern = "{n}/{N} ({p}%)")
inline_text(t1, variable = grade, column = "p.value")



cleanEx()
nameEx("inline_text.tbl_survfit")
### * inline_text.tbl_survfit

flush(stderr()); flush(stdout())

### Name: inline_text.tbl_survfit
### Title: Report statistics from survfit tables inline
### Aliases: inline_text.tbl_survfit

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("survival", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)

# fit survfit
fit1 <- survfit(Surv(ttdeath, death) ~ trt, trial)
fit2 <- survfit(Surv(ttdeath, death) ~ 1, trial)

# sumarize survfit objects
tbl1 <-
  tbl_survfit(
    fit1,
    times = c(12, 24),
    label = ~"Treatment",
    label_header = "**{time} Month**"
  ) %>%
  add_p()

tbl2 <-
  tbl_survfit(
    fit2,
    probs = 0.5,
    label_header = "**Median Survival**"
  )

# report results inline
inline_text(tbl1, time = 24, level = "Drug B")
inline_text(tbl1, time = 24, level = "Drug B",
            pattern = "{estimate} [95% CI {conf.low}, {conf.high}]")
inline_text(tbl1, column = p.value)
inline_text(tbl2, prob = 0.5)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("inline_text.tbl_uvregression")
### * inline_text.tbl_uvregression

flush(stderr()); flush(stdout())

### Name: inline_text.tbl_uvregression
### Title: Report statistics from regression summary tables inline
### Aliases: inline_text.tbl_uvregression

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
inline_text_ex1 <-
  trial[c("response", "age", "grade")] %>%
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = response,
    exponentiate = TRUE
  )

inline_text(inline_text_ex1, variable = age)
inline_text(inline_text_ex1, variable = grade, level = "III")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("is_date_time")
### * is_date_time

flush(stderr()); flush(stdout())

### Name: is_date_time
### Title: Is a date/time
### Aliases: is_date_time
### Keywords: internal

### ** Examples

iris |>
  dplyr::mutate(date = as.Date("2000-01-01") + dplyr::row_number()) |>
  lapply(gtsummary:::is_date_time)



cleanEx()
nameEx("kableExtra_utils")
### * kableExtra_utils

flush(stderr()); flush(stdout())

### Name: kableExtra_utils
### Title: Special Character Escape
### Aliases: kableExtra_utils .escape_html .escape_latex .escape_latex2
### Keywords: internal

### ** Examples

.escape_latex(c("%", "{test}"))
.escape_html(c(">0.9", "line\nbreak"))



cleanEx()
nameEx("label_style")
### * label_style

flush(stderr()); flush(stdout())

### Name: label_style
### Title: Style Functions
### Aliases: label_style label_style_number label_style_sigfig
###   label_style_pvalue label_style_ratio label_style_percent

### ** Examples

my_style <- label_style_number(digits = 1)
my_style(3.14)



cleanEx()
nameEx("modify")
### * modify

flush(stderr()); flush(stdout())

### Name: modify
### Title: Modify column headers, footnotes, and spanning headers
### Aliases: modify modify_header modify_footnote modify_spanning_header
###   show_header_names

### ** Examples

# create summary table
tbl <- trial |>
  tbl_summary(by = trt, missing = "no", include = c("age", "grade", "trt")) |>
  add_p()

# print the column names that can be modified
show_header_names(tbl)

# Example 1 ----------------------------------
# updating column headers and footnote
tbl |>
  modify_header(label = "**Variable**", p.value = "**P**") |>
  modify_footnote(all_stat_cols() ~ "median (IQR) for Age; n (%) for Grade")

# Example 2 ----------------------------------
# updating headers, remove all footnotes, add spanning header
tbl |>
  modify_header(all_stat_cols() ~ "**{level}**, N = {n} ({style_percent(p)}%)") |>
  modify_footnote(everything() ~ NA) |>
  modify_spanning_header(all_stat_cols() ~ "**Treatment Received**")

# Example 3 ----------------------------------
# updating an abbreviation in table footnote
glm(response ~ age + grade, trial, family = binomial) |>
  tbl_regression(exponentiate = TRUE) |>
  modify_footnote(conf.low = "CI = Credible Interval", abbreviation = TRUE)



cleanEx()
nameEx("modify_caption")
### * modify_caption

flush(stderr()); flush(stdout())

### Name: modify_caption
### Title: Modify table caption
### Aliases: modify_caption

### ** Examples

trial |>
  tbl_summary(by = trt, include = c(marker, stage)) |>
  modify_caption(caption = "**Baseline Characteristics** N = {N}")



cleanEx()
nameEx("modify_column_alignment")
### * modify_column_alignment

flush(stderr()); flush(stdout())

### Name: modify_column_alignment
### Title: Modify column alignment
### Aliases: modify_column_alignment

### ** Examples

# Example 1 ----------------------------------
lm(age ~ marker + grade, trial) %>%
  tbl_regression() %>%
  modify_column_alignment(columns = everything(), align = "left")



cleanEx()
nameEx("modify_column_hide")
### * modify_column_hide

flush(stderr()); flush(stdout())

### Name: modify_column_hide
### Title: Modify hidden columns
### Aliases: modify_column_hide modify_column_unhide

### ** Examples

# Example 1 ----------------------------------
# hide 95% CI, and replace with standard error
lm(age ~ marker + grade, trial) |>
  tbl_regression() |>
  modify_column_hide(conf.low) |>
  modify_column_unhide(columns = std.error)



cleanEx()
nameEx("modify_column_indent")
### * modify_column_indent

flush(stderr()); flush(stdout())

### Name: modify_column_indent
### Title: Modify column indentation
### Aliases: modify_column_indent

### ** Examples

# remove indentation from `tbl_summary()`
trial |>
  tbl_summary(include = grade) |>
  modify_column_indent(columns = label, indent = 0L)

# increase indentation in `tbl_summary`
trial |>
  tbl_summary(include = grade) |>
  modify_column_indent(columns = label, rows = !row_type %in% 'label', indent = 8L)



cleanEx()
nameEx("modify_column_merge")
### * modify_column_merge

flush(stderr()); flush(stdout())

### Name: modify_column_merge
### Title: Modify Column Merging
### Aliases: modify_column_merge

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("broom", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, missing = "no", include = c(age, marker, trt)) |>
  add_p(all_continuous() ~ "t.test", pvalue_fun = label_style_pvalue(prepend_p = TRUE)) |>
  modify_fmt_fun(statistic ~ label_style_sigfig()) |>
  modify_column_merge(pattern = "t = {statistic}; {p.value}") |>
  modify_header(statistic = "**t-test**")

# Example 2 ----------------------------------
lm(marker ~ age + grade, trial) |>
  tbl_regression() |>
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("modify_fmt_fun")
### * modify_fmt_fun

flush(stderr()); flush(stdout())

### Name: modify_fmt_fun
### Title: Modify formatting functions
### Aliases: modify_fmt_fun

### ** Examples

# Example 1 ----------------------------------
# show 'grade' p-values to 3 decimal places and estimates to 4 sig figs
lm(age ~ marker + grade, trial) |>
  tbl_regression() %>%
  modify_fmt_fun(
    p.value = label_style_pvalue(digits = 3),
    c(estimate, conf.low, conf.high) ~ label_style_sigfig(digits = 4),
    rows = variable == "grade"
  )



cleanEx()
nameEx("modify_table_body")
### * modify_table_body

flush(stderr()); flush(stdout())

### Name: modify_table_body
### Title: Modify Table Body
### Aliases: modify_table_body

### ** Examples

# Example 1 --------------------------------
# Add number of cases and controls to regression table
trial |>
 tbl_uvregression(
   y = response,
   include = c(age, marker),
   method = glm,
   method.args = list(family = binomial),
   exponentiate = TRUE,
   hide_n = TRUE
 ) |>
 # adding number of non-events to table
 modify_table_body(
   ~ .x %>%
     dplyr::mutate(N_nonevent = N_obs - N_event) |>
     dplyr::relocate(c(N_event, N_nonevent), .before = estimate)
 ) |>
 # assigning header labels
 modify_header(N_nonevent = "**Control N**", N_event = "**Case N**") |>
 modify_fmt_fun(c(N_event, N_nonevent) ~ style_number)



cleanEx()
nameEx("plot")
### * plot

flush(stderr()); flush(stdout())

### Name: plot
### Title: Plot Regression Coefficients
### Aliases: plot plot.tbl_regression plot.tbl_uvregression

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("ggstats", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
glm(response ~ marker + grade, trial, family = binomial) |>
  tbl_regression(
    add_estimate_to_reference_rows = TRUE,
    exponentiate = TRUE
  ) |>
  plot()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("proportion_summary")
### * proportion_summary

flush(stderr()); flush(stdout())

### Name: proportion_summary
### Title: Summarize a proportion
### Aliases: proportion_summary

### ** Examples

# Example 1 ----------------------------------
Titanic |>
  as.data.frame() |>
  tbl_custom_summary(
    include = c("Age", "Class"),
    by = "Sex",
    stat_fns = ~ proportion_summary("Survived", "Yes", weights = "Freq"),
    statistic = ~ "{prop}% ({n}/{N}) [{conf.low}-{conf.high}]",
    digits = ~ list(
      prop = label_style_percent(digits = 1),
      n = 0,
      N = 0,
      conf.low = label_style_percent(),
      conf.high = label_style_percent()
    ),
    overall_row = TRUE,
    overall_row_last = TRUE
  ) |>
  bold_labels() |>
  modify_footnote(all_stat_cols() ~ "Proportion (%) of survivors (n/N) [95% CI]")



cleanEx()
nameEx("ratio_summary")
### * ratio_summary

flush(stderr()); flush(stdout())

### Name: ratio_summary
### Title: Summarize the ratio of two variables
### Aliases: ratio_summary

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_custom_summary(
    include = c("stage", "grade"),
    by = "trt",
    stat_fns = ~ ratio_summary("response", "ttdeath"),
    statistic = ~"{ratio} [{conf.low}; {conf.high}] ({num}/{denom})",
    digits = ~ c(ratio = 3, conf.low = 2, conf.high = 2),
    overall_row = TRUE,
    overall_row_label = "All stages & grades"
  ) |>
  bold_labels() |>
  modify_footnote(all_stat_cols() ~ "Ratio [95% CI] (n/N)")



cleanEx()
nameEx("remove_row_type")
### * remove_row_type

flush(stderr()); flush(stdout())

### Name: remove_row_type
### Title: Remove rows
### Aliases: remove_row_type

### ** Examples

# Example 1 ----------------------------------
trial |>
  dplyr::mutate(
    age60 = ifelse(age < 60, "<60", "60+")
  ) |>
  tbl_summary(by = trt, missing = "no", include = c(trt, age, age60)) |>
  remove_row_type(age60, type = "header")



cleanEx()
nameEx("scoping_gtsummary")
### * scoping_gtsummary

flush(stderr()); flush(stdout())

### Name: scoping_gtsummary
### Title: Scoping for Table Body and Header
### Aliases: scoping_gtsummary scope_table_body scope_header
### Keywords: internal

### ** Examples

tbl <- tbl_summary(trial, include = c(age, grade))

scope_table_body(tbl$table_body) |> select(all_continuous()) |> names()



cleanEx()
nameEx("select_helpers")
### * select_helpers

flush(stderr()); flush(stdout())

### Name: select_helpers
### Title: Select helper functions
### Aliases: select_helpers all_continuous all_continuous2 all_categorical
###   all_dichotomous all_tests all_intercepts all_interaction
###   all_contrasts all_stat_cols

### ** Examples

select_ex1 <-
  trial |>
  select(age, response, grade) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_dichotomous() ~ "categorical"
  )



cleanEx()
nameEx("separate_p_footnotes")
### * separate_p_footnotes

flush(stderr()); flush(stdout())

### Name: separate_p_footnotes
### Title: Create footnotes for individual p-values
### Aliases: separate_p_footnotes

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_summary(by = trt, include = c(age, grade)) |>
  add_p() |>
  separate_p_footnotes()



cleanEx()
nameEx("set_gtsummary_theme")
### * set_gtsummary_theme

flush(stderr()); flush(stdout())

### Name: set_gtsummary_theme
### Title: Set gtsummary theme
### Aliases: set_gtsummary_theme reset_gtsummary_theme get_gtsummary_theme
###   with_gtsummary_theme check_gtsummary_theme

### ** Examples

# Setting JAMA theme for gtsummary
set_gtsummary_theme(theme_gtsummary_journal("jama"))
# Themes can be combined by including more than one
set_gtsummary_theme(theme_gtsummary_compact())

set_gtsummary_theme_ex1 <-
  trial |>
  tbl_summary(by = trt, include = c(age, grade, trt)) |>
  add_stat_label() |>
  as_gt()

# reset gtsummary theme
reset_gtsummary_theme()



cleanEx()
nameEx("sort_filter_p")
### * sort_filter_p

flush(stderr()); flush(stdout())

### Name: sort_filter_p
### Title: Sort/filter by p-values
### Aliases: sort_filter_p sort_p filter_p

### ** Examples

# Example 1 ----------------------------------
trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(by = trt) %>%
  add_p() %>%
  filter_p(t = 0.8) %>%
  sort_p()

# Example 2 ----------------------------------
glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
  tbl_regression(exponentiate = TRUE) %>%
  sort_p()



cleanEx()
nameEx("style_number")
### * style_number

flush(stderr()); flush(stdout())

### Name: style_number
### Title: Style numbers
### Aliases: style_number

### ** Examples

c(0.111, 12.3) |> style_number(digits = 1)
c(0.111, 12.3) |> style_number(digits = c(1, 0))



cleanEx()
nameEx("style_percent")
### * style_percent

flush(stderr()); flush(stdout())

### Name: style_percent
### Title: Style percentages
### Aliases: style_percent

### ** Examples

percent_vals <- c(-1, 0, 0.0001, 0.005, 0.01, 0.10, 0.45356, 0.99, 1.45)
style_percent(percent_vals)
style_percent(percent_vals, symbol = TRUE, digits = 1)



cleanEx()
nameEx("style_pvalue")
### * style_pvalue

flush(stderr()); flush(stdout())

### Name: style_pvalue
### Title: Style p-values
### Aliases: style_pvalue

### ** Examples

pvals <- c(
  1.5, 1, 0.999, 0.5, 0.25, 0.2, 0.197, 0.12, 0.10, 0.0999, 0.06,
  0.03, 0.002, 0.001, 0.00099, 0.0002, 0.00002, -1
)
style_pvalue(pvals)
style_pvalue(pvals, digits = 2, prepend_p = TRUE)



cleanEx()
nameEx("style_ratio")
### * style_ratio

flush(stderr()); flush(stdout())

### Name: style_ratio
### Title: Style ratios
### Aliases: style_ratio

### ** Examples

c(0.123, 0.9, 1.1234, 12.345, 101.234, -0.123, -0.9, -1.1234, -12.345, -101.234) |>
  style_ratio()



cleanEx()
nameEx("style_sigfig")
### * style_sigfig

flush(stderr()); flush(stdout())

### Name: style_sigfig
### Title: Style significant figure-like rounding
### Aliases: style_sigfig

### ** Examples

c(0.123, 0.9, 1.1234, 12.345, -0.123, -0.9, -1.1234, -132.345, NA, -0.001) %>%
  style_sigfig()



cleanEx()
nameEx("tbl_ard_continuous")
### * tbl_ard_continuous

flush(stderr()); flush(stdout())

### Name: tbl_ard_continuous
### Title: Summarize continuous variable
### Aliases: tbl_ard_continuous

### ** Examples

library(cards)

bind_ard(
  # the primary ARD with the results
  ard_continuous(
    trial,
    # the order variables are passed here is important.
    # 'trt' is the column stratifying variable and needs to be listed first.
    by = c(trt, grade),
    variables = age
  ) ,
  # add univariate trt tabulation
  ard_categorical(
    trial,
    variables = trt
  ),
  # add missing and attributes ARD
  ard_missing(
    trial,
    by = c(trt, grade),
    variables = age
  ),
  ard_attributes(
    trial,
    variables = c(trt, grade, age)
  )
) |>
  tbl_ard_continuous(by = "trt", variable = "age", include = "grade")

bind_ard(
  # the primary ARD with the results
  ard_continuous(trial, by = grade, variables = age),
  # add missing and attributes ARD
  ard_missing(trial, by = grade, variables = age),
  ard_attributes(trial, variables = c(grade, age))
) |>
  tbl_ard_continuous(variable = "age", include = "grade")



cleanEx()
nameEx("tbl_ard_summary")
### * tbl_ard_summary

flush(stderr()); flush(stdout())

### Name: tbl_ard_summary
### Title: ARD summary table
### Aliases: tbl_ard_summary

### ** Examples

library(cards)

ard_stack(
  data = ADSL,
  ard_categorical(variables = "AGEGR1"),
  ard_continuous(variables = "AGE"),
  .attributes = TRUE,
  .missing = TRUE
) |>
  tbl_ard_summary()

ard_stack(
  data = ADSL,
  .by = ARM,
  ard_categorical(variables = "AGEGR1"),
  ard_continuous(variables = "AGE"),
  .attributes = TRUE,
  .missing = TRUE
) |>
  tbl_ard_summary(by = ARM)



cleanEx()
nameEx("tbl_ard_wide_summary")
### * tbl_ard_wide_summary

flush(stderr()); flush(stdout())

### Name: tbl_ard_wide_summary
### Title: Wide ARD summary table
### Aliases: tbl_ard_wide_summary

### ** Examples

library(cards)

ard_stack(
  trial,
  ard_continuous(variables = age),
  .missing = TRUE,
  .attributes = TRUE
) |>
  tbl_ard_wide_summary()

ard_stack(
  trial,
  ard_dichotomous(variables = response),
  ard_categorical(variables = grade),
  .missing = TRUE,
  .attributes = TRUE
) |>
  tbl_ard_wide_summary()



cleanEx()
nameEx("tbl_butcher")
### * tbl_butcher

flush(stderr()); flush(stdout())

### Name: tbl_butcher
### Title: Butcher table
### Aliases: tbl_butcher

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
tbl_large <-
  trial |>
  tbl_uvregression(
    y = age,
    method = lm
  )

tbl_butchered <-
  tbl_large |>
  tbl_butcher()

# size comparison
object.size(tbl_large) |> format(units = "Mb")
object.size(tbl_butchered)|> format(units = "Mb")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_continuous")
### * tbl_continuous

flush(stderr()); flush(stdout())

### Name: tbl_continuous
### Title: Summarize continuous variable
### Aliases: tbl_continuous

### ** Examples

# Example 1 ----------------------------------
tbl_continuous(
  data = trial,
  variable = age,
  by = trt,
  include = grade
)

# Example 2 ----------------------------------
tbl_continuous(
  data = trial,
  variable = age,
  statistic = ~"{mean} ({sd})",
  by = trt,
  include = c(stage, grade)
)



cleanEx()
nameEx("tbl_cross")
### * tbl_cross

flush(stderr()); flush(stdout())

### Name: tbl_cross
### Title: Cross table
### Aliases: tbl_cross

### ** Examples

# Example 1 ----------------------------------
trial |>
  tbl_cross(row = trt, col = response) |>
  bold_labels()

# Example 2 ----------------------------------
trial |>
  tbl_cross(row = stage, col = trt, percent = "cell") |>
  add_p() |>
  bold_labels()



cleanEx()
nameEx("tbl_custom_summary")
### * tbl_custom_summary

flush(stderr()); flush(stdout())

### Name: tbl_custom_summary
### Title: Create a table of summary statistics using a custom summary
###   function
### Aliases: tbl_custom_summary

### ** Examples

# Example 1 ----------------------------------
my_stats <- function(data, ...) {
  marker_sum <- sum(data$marker, na.rm = TRUE)
  mean_age <- mean(data$age, na.rm = TRUE)
  dplyr::tibble(
    marker_sum = marker_sum,
    mean_age = mean_age
  )
}

my_stats(trial)

trial |>
  tbl_custom_summary(
    include = c("stage", "grade"),
    by = "trt",
    stat_fns = everything() ~ my_stats,
    statistic = everything() ~ "A: {mean_age} - S: {marker_sum}",
    digits = everything() ~ c(1, 0),
    overall_row = TRUE,
    overall_row_label = "All stages & grades"
  ) |>
  add_overall(last = TRUE) |>
  modify_footnote(
    all_stat_cols() ~ "A: mean age - S: sum of marker"
  ) |>
  bold_labels()

# Example 2 ----------------------------------
# Use `data[[variable]]` to access the current variable
mean_ci <- function(data, variable, ...) {
  test <- t.test(data[[variable]])
  dplyr::tibble(
    mean = test$estimate,
    conf.low = test$conf.int[1],
    conf.high = test$conf.int[2]
  )
}

trial |>
  tbl_custom_summary(
    include = c("marker", "ttdeath"),
    by = "trt",
    stat_fns = ~ mean_ci,
    statistic = ~ "{mean} [{conf.low}; {conf.high}]"
  ) |>
  add_overall(last = TRUE) |>
  modify_footnote(
    all_stat_cols() ~ "mean [95% CI]"
  )

# Example 3 ----------------------------------
# Use `full_data` to access the full datasets
# Returned statistic can also be a character
diff_to_great_mean <- function(data, full_data, ...) {
  mean <- mean(data$marker, na.rm = TRUE)
  great_mean <- mean(full_data$marker, na.rm = TRUE)
  diff <- mean - great_mean
  dplyr::tibble(
    mean = mean,
    great_mean = great_mean,
    diff = diff,
    level = ifelse(diff > 0, "high", "low")
  )
}

trial |>
  tbl_custom_summary(
    include = c("grade", "stage"),
    by = "trt",
    stat_fns = ~ diff_to_great_mean,
    statistic = ~ "{mean} ({level}, diff: {diff})",
    overall_row = TRUE
  ) |>
  bold_labels()



cleanEx()
nameEx("tbl_merge")
### * tbl_merge

flush(stderr()); flush(stdout())

### Name: tbl_merge
### Title: Merge tables
### Aliases: tbl_merge

### ** Examples

## Don't show: 
if ((identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed('survival', reference_pkg = 'gtsummary')) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
# Side-by-side Regression Models
library(survival)

t1 <-
  glm(response ~ trt + grade + age, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(exponentiate = TRUE)

tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)

# Example 2 ----------------------------------
# Descriptive statistics alongside univariate regression, with no spanning header
t3 <-
  trial[c("age", "grade", "response")] %>%
  tbl_summary(missing = "no") %>%
  add_n() %>%
  modify_header(stat_0 ~ "**Summary Statistics**")
t4 <-
  tbl_uvregression(
    trial[c("ttdeath", "death", "age", "grade", "response")],
    method = coxph,
    y = Surv(ttdeath, death),
    exponentiate = TRUE,
    hide_n = TRUE
  )

tbl_merge(tbls = list(t3, t4)) %>%
  modify_spanning_header(everything() ~ NA_character_)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_regression")
### * tbl_regression

flush(stderr()); flush(stdout())

### Name: tbl_regression
### Title: Regression model summary
### Aliases: tbl_regression tbl_regression.default

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
glm(response ~ age + grade, trial, family = binomial()) |>
  tbl_regression(exponentiate = TRUE)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_split")
### * tbl_split

flush(stderr()); flush(stdout())

### Name: tbl_split
### Title: Split gtsummary table
### Aliases: tbl_split tbl_split.gtsummary print.tbl_split

### ** Examples

tbl <-
  tbl_summary(trial) |>
  tbl_split(variables = c(marker, grade))



cleanEx()
nameEx("tbl_stack")
### * tbl_stack

flush(stderr()); flush(stdout())

### Name: tbl_stack
### Title: Stack tables
### Aliases: tbl_stack

### ** Examples

## Don't show: 
if ((identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed("cardx", reference_pkg = "gtsummary") && gtsummary:::is_pkg_installed("survival", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
# stacking two tbl_regression objects
t1 <-
  glm(response ~ trt, trial, family = binomial) %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (unadjusted)")
  )

t2 <-
  glm(response ~ trt + grade + stage + marker, trial, family = binomial) %>%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (adjusted)")
  )

tbl_stack(list(t1, t2))

# Example 2 ----------------------------------
# stacking two tbl_merge objects
library(survival)
t3 <-
  coxph(Surv(ttdeath, death) ~ trt, trial) %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (unadjusted)")
  )

t4 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) %>%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list(trt ~ "Treatment (adjusted)")
  )

# first merging, then stacking
row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
row2 <- tbl_merge(list(t2, t4))

tbl_stack(list(row1, row2), group_header = c("Unadjusted Analysis", "Adjusted Analysis"))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_strata")
### * tbl_strata

flush(stderr()); flush(stdout())

### Name: tbl_strata
### Title: Stratified gtsummary tables
### Aliases: tbl_strata tbl_strata2

### ** Examples

# Example 1 ----------------------------------
trial |>
  select(age, grade, stage, trt) |>
  mutate(grade = paste("Grade", grade)) |>
  tbl_strata(
    strata = grade,
    .tbl_fun =
      ~ .x |>
        tbl_summary(by = trt, missing = "no") |>
        add_n(),
    .header = "**{strata}**, N = {n}"
  )

# Example 2 ----------------------------------
trial |>
  select(grade, response) |>
  mutate(grade = paste("Grade", grade)) |>
  tbl_strata2(
    strata = grade,
    .tbl_fun =
      ~ .x %>%
        tbl_summary(
          label = list(response = .y),
          missing = "no",
          statistic = response ~ "{p}%"
        ) |>
        add_ci(pattern = "{stat} ({ci})") |>
        modify_header(stat_0 = "**Rate (95% CI)**") |>
        modify_footnote(stat_0 = NA),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL),
    .quiet = TRUE
  ) |>
  modify_caption("**Response Rate by Grade**")



cleanEx()
nameEx("tbl_summary")
### * tbl_summary

flush(stderr()); flush(stdout())

### Name: tbl_summary
### Title: Summary table
### Aliases: tbl_summary

### ** Examples

## Don't show: 
if (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
trial |>
  select(age, grade, response) |>
  tbl_summary()

# Example 2 ----------------------------------
trial |>
  select(age, grade, response, trt) |>
  tbl_summary(
    by = trt,
    label = list(age = "Patient Age"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(age = c(0, 1))
  )

# Example 3 ----------------------------------
trial |>
  select(age, marker) |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
    missing = "no"
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_survfit")
### * tbl_survfit

flush(stderr()); flush(stdout())

### Name: tbl_survfit
### Title: Survival table
### Aliases: tbl_survfit tbl_survfit.survfit tbl_survfit.data.frame
###   tbl_survfit.list

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed("survival", reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)

# Example 1 ----------------------------------
# Pass single survfit() object
tbl_survfit(
  survfit(Surv(ttdeath, death) ~ trt, trial),
  times = c(12, 24),
  label_header = "**{time} Month**"
)

# Example 2 ----------------------------------
# Pass a data frame
tbl_survfit(
  trial,
  y = "Surv(ttdeath, death)",
  include = c(trt, grade),
  probs = 0.5,
  label_header = "**Median Survival**"
)

# Example 3 ----------------------------------
# Pass a list of survfit() objects
list(survfit(Surv(ttdeath, death) ~ 1, trial),
     survfit(Surv(ttdeath, death) ~ trt, trial)) |>
  tbl_survfit(times = c(12, 24))

# Example 4 Competing Events Example ---------
# adding a competing event for death (cancer vs other causes)
set.seed(1123)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
trial2 <- trial |>
  dplyr::mutate(
    death_cr =
      dplyr::case_when(
        death == 0 ~ "censor",
        runif(n()) < 0.5 ~ "death from cancer",
        TRUE ~ "death other causes"
      ) |>
      factor()
  )

survfit(Surv(ttdeath, death_cr) ~ grade, data = trial2) |>
  tbl_survfit(times = c(12, 24), label = "Tumor Grade")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_svysummary")
### * tbl_svysummary

flush(stderr()); flush(stdout())

### Name: tbl_svysummary
### Title: Create a table of summary statistics from a survey object
### Aliases: tbl_svysummary

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "survey"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq) |>
  tbl_svysummary(by = Survived, percent = "row", include = c(Class, Age))

# Example 2 ----------------------------------
# A dataset with a complex design
data(api, package = "survey")
survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc) |>
  tbl_svysummary(by = "both", include = c(api00, stype)) |>
  modify_spanning_header(all_stat_cols() ~ "**Survived**")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_uvregression")
### * tbl_uvregression

flush(stderr()); flush(stdout())

### Name: tbl_uvregression
### Title: Univariable regression model summary
### Aliases: tbl_uvregression tbl_uvregression.data.frame
###   tbl_uvregression.survey.design

### ** Examples

## Don't show: 
if (gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Example 1 ----------------------------------
tbl_uvregression(
  trial,
  method = glm,
  y = response,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  include = c("age", "grade")
)

# Example 2 ----------------------------------
# rounding pvalues to 2 decimal places
library(survival)

tbl_uvregression(
  trial,
  method = coxph,
  y = Surv(ttdeath, death),
  exponentiate = TRUE,
  include = c("age", "grade", "response"),
  pvalue_fun = label_style_pvalue(digits = 2)
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tbl_wide_summary")
### * tbl_wide_summary

flush(stderr()); flush(stdout())

### Name: tbl_wide_summary
### Title: Wide summary table
### Aliases: tbl_wide_summary

### ** Examples

trial |>
  tbl_wide_summary(include = c(response, grade))

trial |>
  tbl_strata(
    strata = trt,
    ~tbl_wide_summary(.x, include = c(age, marker))
  )



cleanEx()
nameEx("theme_gtsummary")
### * theme_gtsummary

flush(stderr()); flush(stdout())

### Name: theme_gtsummary
### Title: Available gtsummary themes
### Aliases: theme_gtsummary theme_gtsummary_journal
###   theme_gtsummary_compact theme_gtsummary_printer
###   theme_gtsummary_language theme_gtsummary_continuous2
###   theme_gtsummary_mean_sd theme_gtsummary_eda

### ** Examples

# Setting JAMA theme for gtsummary
theme_gtsummary_journal("jama")
# Themes can be combined by including more than one
theme_gtsummary_compact()

trial |>
  select(age, grade, trt) |>
  tbl_summary(by = trt) |>
  as_gt()

# reset gtsummary themes
reset_gtsummary_theme()



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
