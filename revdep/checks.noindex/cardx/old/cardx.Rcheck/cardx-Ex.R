pkgname <- "cardx"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('cardx')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ard_aod_wald_test")
### * ard_aod_wald_test

flush(stderr()); flush(stdout())

### Name: ard_aod_wald_test
### Title: ARD Wald Test
### Aliases: ard_aod_wald_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "aod", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
lm(AGE ~ ARM, data = cards::ADSL) |>
  ard_aod_wald_test()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_attributes")
### * ard_attributes

flush(stderr()); flush(stdout())

### Name: ard_attributes.survey.design
### Title: ARD Attributes
### Aliases: ard_attributes.survey.design

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

ard_attributes(
  data = dclus1,
  variables = c(sname, dname),
  label = list(sname = "School Name", dname = "District Name")
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_car_anova")
### * ard_car_anova

flush(stderr()); flush(stdout())

### Name: ard_car_anova
### Title: ARD ANOVA from car Package
### Aliases: ard_car_anova

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("broom.helpers", "car", "parameters"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
lm(AGE ~ ARM, data = cards::ADSL) |>
  ard_car_anova()

glm(vs ~ factor(cyl) + factor(am), data = mtcars, family = binomial) |>
  ard_car_anova(test.statistic = "Wald")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_car_vif")
### * ard_car_vif

flush(stderr()); flush(stdout())

### Name: ard_car_vif
### Title: Regression VIF ARD
### Aliases: ard_car_vif

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "car", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
lm(AGE ~ ARM + SEX, data = cards::ADSL) |>
  ard_car_vif()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_categorical.survey.design")
### * ard_categorical.survey.design

flush(stderr()); flush(stdout())

### Name: ard_categorical.survey.design
### Title: ARD Categorical Survey Statistics
### Aliases: ard_categorical.survey.design

### ** Examples

## Don't show: 
if (cardx:::is_pkg_installed("survey", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

ard_categorical(svy_titanic, variables = c(Class, Age), by = Survived)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_categorical_ci")
### * ard_categorical_ci

flush(stderr()); flush(stdout())

### Name: ard_categorical_ci
### Title: ARD Proportion Confidence Intervals
### Aliases: ard_categorical_ci ard_categorical_ci.data.frame

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# compute CI for binary variables
ard_categorical_ci(mtcars, variables = c(vs, am), method = "wilson")

# compute CIs for each level of a categorical variable
ard_categorical_ci(mtcars, variables = cyl, method = "jeffreys")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_categorical_ci.survey.design")
### * ard_categorical_ci.survey.design

flush(stderr()); flush(stdout())

### Name: ard_categorical_ci.survey.design
### Title: ARD survey categorical CIs
### Aliases: ard_categorical_ci.survey.design

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

ard_categorical_ci(dclus1, variables = sch.wide)
ard_categorical_ci(dclus1, variables = sch.wide, value = sch.wide ~ "Yes", method = "xlogit")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_continuous.survey.design")
### * ard_continuous.survey.design

flush(stderr()); flush(stdout())

### Name: ard_continuous.survey.design
### Title: ARD Continuous Survey Statistics
### Aliases: ard_continuous.survey.design

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

ard_continuous(
  data = dclus1,
  variables = api00,
  by = stype
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_continuous_ci")
### * ard_continuous_ci

flush(stderr()); flush(stdout())

### Name: ard_continuous_ci
### Title: ARD continuous CIs
### Aliases: ard_continuous_ci ard_continuous_ci.data.frame

### ** Examples

ard_continuous_ci(mtcars, variables = c(mpg, hp), method = "wilcox.test")
ard_continuous_ci(mtcars, variables = mpg, by = am, method = "t.test")



cleanEx()
nameEx("ard_continuous_ci.survey.design")
### * ard_continuous_ci.survey.design

flush(stderr()); flush(stdout())

### Name: ard_continuous_ci.survey.design
### Title: ARD survey continuous CIs
### Aliases: ard_continuous_ci.survey.design

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survey", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

ard_continuous_ci(dclus1, variables = api00)
ard_continuous_ci(dclus1, variables = api00, method = "svymedian.xlogit")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_dichotomous.survey.design")
### * ard_dichotomous.survey.design

flush(stderr()); flush(stdout())

### Name: ard_dichotomous.survey.design
### Title: ARD Dichotomous Survey Statistics
### Aliases: ard_dichotomous.survey.design

### ** Examples

## Don't show: 
if (cardx:::is_pkg_installed("survey", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
survey::svydesign(ids = ~1, data = mtcars, weights = ~1) |>
  ard_dichotomous(by = vs, variables = c(cyl, am), value = list(cyl = 4))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_effectsize_cohens_d")
### * ard_effectsize_cohens_d

flush(stderr()); flush(stdout())

### Name: ard_effectsize_cohens_d
### Title: ARD Cohen's D Test
### Aliases: ard_effectsize_cohens_d ard_effectsize_paired_cohens_d

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("effectsize", "parameters"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  ard_effectsize_cohens_d(by = ARM, variables = AGE)

# constructing a paired data set,
# where patients receive both treatments
cards::ADSL[c("ARM", "AGE")] |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
  dplyr::arrange(USUBJID, ARM) |>
  dplyr::group_by(USUBJID) |>
  dplyr::filter(dplyr::n() > 1) |>
  ard_effectsize_paired_cohens_d(by = ARM, variables = AGE, id = USUBJID)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_effectsize_hedges_g")
### * ard_effectsize_hedges_g

flush(stderr()); flush(stdout())

### Name: ard_effectsize_hedges_g
### Title: ARD Hedge's G Test
### Aliases: ard_effectsize_hedges_g ard_effectsize_paired_hedges_g

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("effectsize", "parameters"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  ard_effectsize_hedges_g(by = ARM, variables = AGE)

# constructing a paired data set,
# where patients receive both treatments
cards::ADSL[c("ARM", "AGE")] |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
  dplyr::arrange(USUBJID, ARM) |>
  dplyr::group_by(USUBJID) |>
  dplyr::filter(dplyr::n() > 1) |>
  ard_effectsize_paired_hedges_g(by = ARM, variables = AGE, id = USUBJID)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_emmeans_mean_difference")
### * ard_emmeans_mean_difference

flush(stderr()); flush(stdout())

### Name: ard_emmeans_mean_difference
### Title: ARD for LS Mean Difference
### Aliases: ard_emmeans_mean_difference

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "emmeans", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
ard_emmeans_mean_difference(
  data = mtcars,
  formula = mpg ~ am + cyl,
  method = "lm"
)

ard_emmeans_mean_difference(
  data = mtcars,
  formula = vs ~ am + mpg,
  method = "glm",
  method.args = list(family = binomial),
  response_type = "dichotomous"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_missing.survey.design")
### * ard_missing.survey.design

flush(stderr()); flush(stdout())

### Name: ard_missing.survey.design
### Title: ARD Missing Survey Statistics
### Aliases: ard_missing.survey.design

### ** Examples

## Don't show: 
if (cardx:::is_pkg_installed("survey", reference_pkg = "cardx")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
svy_titanic <- survey::svydesign(~1, data = as.data.frame(Titanic), weights = ~Freq)

ard_missing(svy_titanic, variables = c(Class, Age), by = Survived)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_regression")
### * ard_regression

flush(stderr()); flush(stdout())

### Name: ard_regression
### Title: Regression ARD
### Aliases: ard_regression ard_regression.default

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom.helpers", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
lm(AGE ~ ARM, data = cards::ADSL) |>
  ard_regression(add_estimate_to_reference_rows = TRUE)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_regression_basic")
### * ard_regression_basic

flush(stderr()); flush(stdout())

### Name: ard_regression_basic
### Title: Basic Regression ARD
### Aliases: ard_regression_basic

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom.helpers", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
lm(AGE ~ ARM, data = cards::ADSL) |>
  ard_regression_basic()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_smd_smd")
### * ard_smd_smd

flush(stderr()); flush(stdout())

### Name: ard_smd_smd
### Title: ARD Standardized Mean Difference
### Aliases: ard_smd_smd

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "smd", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
ard_smd_smd(cards::ADSL, by = SEX, variables = AGE)
ard_smd_smd(cards::ADSL, by = SEX, variables = AGEGR1)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_anova")
### * ard_stats_anova

flush(stderr()); flush(stdout())

### Name: ard_stats_anova
### Title: ARD ANOVA
### Aliases: ard_stats_anova ard_stats_anova.anova
###   ard_stats_anova.data.frame

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("broom", "withr", "lme4"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
anova(
  lm(mpg ~ am, mtcars),
  lm(mpg ~ am + hp, mtcars)
) |>
  ard_stats_anova()

ard_stats_anova(
  x = mtcars,
  formulas = list(am ~ mpg, am ~ mpg + hp),
  method = "glm",
  method.args = list(family = binomial)
)

ard_stats_anova(
  x = mtcars,
  formulas = list(am ~ 1 + (1 | vs), am ~ mpg + (1 | vs)),
  method = "glmer",
  method.args = list(family = binomial),
  package = "lme4"
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_aov")
### * ard_stats_aov

flush(stderr()); flush(stdout())

### Name: ard_stats_aov
### Title: ARD ANOVA
### Aliases: ard_stats_aov

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("broom.helpers", "parameters"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
ard_stats_aov(AGE ~ ARM, data = cards::ADSL)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_chisq_test")
### * ard_stats_chisq_test

flush(stderr()); flush(stdout())

### Name: ard_stats_chisq_test
### Title: ARD Chi-squared Test
### Aliases: ard_stats_chisq_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  ard_stats_chisq_test(by = "ARM", variables = "AGEGR1")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_fisher_test")
### * ard_stats_fisher_test

flush(stderr()); flush(stdout())

### Name: ard_stats_fisher_test
### Title: ARD Fisher's Exact Test
### Aliases: ard_stats_fisher_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL[1:30, ] |>
  ard_stats_fisher_test(by = "ARM", variables = "AGEGR1")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_kruskal_test")
### * ard_stats_kruskal_test

flush(stderr()); flush(stdout())

### Name: ard_stats_kruskal_test
### Title: ARD Kruskal-Wallis Test
### Aliases: ard_stats_kruskal_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  ard_stats_kruskal_test(by = "ARM", variables = "AGE")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_mcnemar_test")
### * ard_stats_mcnemar_test

flush(stderr()); flush(stdout())

### Name: ard_stats_mcnemar_test
### Title: ARD McNemar's Test
### Aliases: ard_stats_mcnemar_test ard_stats_mcnemar_test_long

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  ard_stats_mcnemar_test(by = "SEX", variables = "EFFFL")

set.seed(1234)
cards::ADSL[c("USUBJID", "TRT01P")] |>
  dplyr::mutate(TYPE = "PLANNED") |>
  dplyr::rename(TRT01 = TRT01P) %>%
  dplyr::bind_rows(dplyr::mutate(., TYPE = "ACTUAL", TRT01 = sample(TRT01))) |>
  ard_stats_mcnemar_test_long(
    by = TYPE,
    variable = TRT01,
    id = USUBJID
  )
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_mood_test")
### * ard_stats_mood_test

flush(stderr()); flush(stdout())

### Name: ard_stats_mood_test
### Title: ARD Mood Test
### Aliases: ard_stats_mood_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  ard_stats_mood_test(by = "SEX", variables = "AGE")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_oneway_test")
### * ard_stats_oneway_test

flush(stderr()); flush(stdout())

### Name: ard_stats_oneway_test
### Title: ARD One-way Test
### Aliases: ard_stats_oneway_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
ard_stats_oneway_test(AGE ~ ARM, data = cards::ADSL)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_prop_test")
### * ard_stats_prop_test

flush(stderr()); flush(stdout())

### Name: ard_stats_prop_test
### Title: ARD 2-sample proportion test
### Aliases: ard_stats_prop_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
mtcars |>
  ard_stats_prop_test(by = vs, variables = am)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_t_test")
### * ard_stats_t_test

flush(stderr()); flush(stdout())

### Name: ard_stats_t_test
### Title: ARD t-test
### Aliases: ard_stats_t_test ard_stats_paired_t_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  ard_stats_t_test(by = ARM, variables = c(AGE, BMIBL))

# constructing a paired data set,
# where patients receive both treatments
cards::ADSL[c("ARM", "AGE")] |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
  dplyr::arrange(USUBJID, ARM) |>
  ard_stats_paired_t_test(by = ARM, variables = AGE, id = USUBJID)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_t_test_onesample")
### * ard_stats_t_test_onesample

flush(stderr()); flush(stdout())

### Name: ard_stats_t_test_onesample
### Title: ARD one-sample t-test
### Aliases: ard_stats_t_test_onesample

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  ard_stats_t_test_onesample(by = ARM, variables = AGE)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_wilcox_test")
### * ard_stats_wilcox_test

flush(stderr()); flush(stdout())

### Name: ard_stats_wilcox_test
### Title: ARD Wilcoxon Rank-Sum Test
### Aliases: ard_stats_wilcox_test ard_stats_paired_wilcox_test

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  ard_stats_wilcox_test(by = "ARM", variables = "AGE")

# constructing a paired data set,
# where patients receive both treatments
cards::ADSL[c("ARM", "AGE")] |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
  dplyr::arrange(USUBJID, ARM) |>
  ard_stats_paired_wilcox_test(by = ARM, variables = AGE, id = USUBJID)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_stats_wilcox_test_onesample")
### * ard_stats_wilcox_test_onesample

flush(stderr()); flush(stdout())

### Name: ard_stats_wilcox_test_onesample
### Title: ARD one-sample Wilcox Rank-sum
### Aliases: ard_stats_wilcox_test_onesample

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cards::ADSL |>
  ard_stats_wilcox_test_onesample(by = ARM, variables = AGE)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_survey_svychisq")
### * ard_survey_svychisq

flush(stderr()); flush(stdout())

### Name: ard_survey_svychisq
### Title: ARD Survey Chi-Square Test
### Aliases: ard_survey_svychisq

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survey", "broom"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

ard_survey_svychisq(dclus1, variables = sch.wide, by = comp.imp, statistic = "F")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_survey_svyranktest")
### * ard_survey_svyranktest

flush(stderr()); flush(stdout())

### Name: ard_survey_svyranktest
### Title: ARD Survey rank test
### Aliases: ard_survey_svyranktest

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survey", "broom"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

ard_survey_svyranktest(dclus2, variables = enroll, by = comp.imp, test = "wilcoxon")
ard_survey_svyranktest(dclus2, variables = enroll, by = comp.imp, test = "vanderWaerden")
ard_survey_svyranktest(dclus2, variables = enroll, by = comp.imp, test = "median")
ard_survey_svyranktest(dclus2, variables = enroll, by = comp.imp, test = "KruskalWallis")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_survey_svyttest")
### * ard_survey_svyttest

flush(stderr()); flush(stdout())

### Name: ard_survey_svyttest
### Title: ARD Survey t-test
### Aliases: ard_survey_svyttest

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survey", "broom"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
data(api, package = "survey")
dclus2 <- survey::svydesign(id = ~ dnum + snum, fpc = ~ fpc1 + fpc2, data = apiclus2)

ard_survey_svyttest(dclus2, variables = enroll, by = comp.imp, conf.level = 0.9)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_survival_survdiff")
### * ard_survival_survdiff

flush(stderr()); flush(stdout())

### Name: ard_survival_survdiff
### Title: ARD for Difference in Survival
### Aliases: ard_survival_survdiff

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom", "ggsurvfit"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)
library(ggsurvfit)

ard_survival_survdiff(Surv_CNSR(AVAL, CNSR) ~ TRTA, data = cards::ADTTE)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_survival_survfit")
### * ard_survival_survfit

flush(stderr()); flush(stdout())

### Name: ard_survival_survfit
### Title: ARD Survival Estimates
### Aliases: ard_survival_survfit

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom", "ggsurvfit"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(survival)
library(ggsurvfit)

survfit(Surv_CNSR(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
  ard_survival_survfit(times = c(60, 180))

survfit(Surv_CNSR(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
  ard_survival_survfit(probs = c(0.25, 0.5, 0.75))

# Competing Risks Example ---------------------------
set.seed(1)
ADTTE_MS <- cards::ADTTE %>%
  dplyr::mutate(
    CNSR = dplyr::case_when(
      CNSR == 0 ~ "censor",
      runif(dplyr::n()) < 0.5 ~ "death from cancer",
      TRUE ~ "death other causes"
    ) %>% factor()
  )

survfit(Surv(AVAL, CNSR) ~ TRTA, data = ADTTE_MS) %>%
  ard_survival_survfit(times = c(60, 180))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("ard_survival_survfit_diff")
### * ard_survival_survfit_diff

flush(stderr()); flush(stdout())

### Name: ard_survival_survfit_diff
### Title: ARD Survival Differences
### Aliases: ard_survival_survfit_diff

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "ggsurvfit"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(ggsurvfit)
library(survival)

survfit(Surv_CNSR() ~ TRTA, data = cards::ADTTE) |>
  ard_survival_survfit_diff(times = c(25, 50))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("construction_helpers")
### * construction_helpers

flush(stderr()); flush(stdout())

### Name: construction_helpers
### Title: Construction Helpers
### Aliases: construction_helpers construct_model
###   construct_model.data.frame construct_model.survey.design reformulate2
###   bt bt_strip

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("withr", "lme4", "broom.helpers", "broom.mixed"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
construct_model(
  data = mtcars,
  formula = am ~ mpg + (1 | vs),
  method = "glmer",
  method.args = list(family = binomial),
  package = "lme4"
) |>
  broom.mixed::tidy()

construct_model(
  data = mtcars |> dplyr::rename(`M P G` = mpg),
  formula = reformulate2(c("M P G", "cyl"), response = "hp"),
  method = "lm"
) |>
  ard_regression() |>
  dplyr::filter(stat_name %in% c("term", "estimate", "p.value"))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-check_dichotomous_value")
### * dot-check_dichotomous_value

flush(stderr()); flush(stdout())

### Name: .check_dichotomous_value
### Title: Perform Value Checks
### Aliases: .check_dichotomous_value
### Keywords: internal

### ** Examples

cardx:::.check_dichotomous_value(mtcars, list(cyl = 4))



cleanEx()
nameEx("dot-format_cohens_d_results")
### * dot-format_cohens_d_results

flush(stderr()); flush(stdout())

### Name: .format_cohens_d_results
### Title: Convert Cohen's D Test to ARD
### Aliases: .format_cohens_d_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("effectsize", "parameters"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cardx:::.format_cohens_d_results(
  by = "ARM",
  variable = "AGE",
  paired = FALSE,
  lst_tidy =
    cards::eval_capture_conditions(
      effectsize::hedges_g(data[[variable]] ~ data[[by]], paired = FALSE) |>
        parameters::standardize_names(style = "broom")
    )
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-format_hedges_g_results")
### * dot-format_hedges_g_results

flush(stderr()); flush(stdout())

### Name: .format_hedges_g_results
### Title: Convert Hedge's G Test to ARD
### Aliases: .format_hedges_g_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("effectsize", "parameters"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cardx:::.format_hedges_g_results(
  by = "ARM",
  variable = "AGE",
  paired = FALSE,
  lst_tidy =
    cards::eval_capture_conditions(
      effectsize::hedges_g(data[[variable]] ~ data[[by]], paired = FALSE) |>
        parameters::standardize_names(style = "broom")
    )
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-format_mcnemartest_results")
### * dot-format_mcnemartest_results

flush(stderr()); flush(stdout())

### Name: .format_mcnemartest_results
### Title: Convert McNemar's test to ARD
### Aliases: .format_mcnemartest_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cardx:::.format_mcnemartest_results(
  by = "ARM",
  variable = "AGE",
  lst_tidy =
    cards::eval_capture_conditions(
      stats::mcnemar.test(cards::ADSL[["SEX"]], cards::ADSL[["EFFFL"]]) |>
        broom::tidy()
    )
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-format_moodtest_results")
### * dot-format_moodtest_results

flush(stderr()); flush(stdout())

### Name: .format_moodtest_results
### Title: Convert mood test results to ARD
### Aliases: .format_moodtest_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cardx:::.format_moodtest_results(
  by = "SEX",
  variable = "AGE",
  lst_tidy =
    cards::eval_capture_conditions(
      stats::mood.test(ADSL[["AGE"]] ~ ADSL[["SEX"]]) |>
        broom::tidy()
    )
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-format_survfit_results")
### * dot-format_survfit_results

flush(stderr()); flush(stdout())

### Name: .format_survfit_results
### Title: Convert Tidied Survival Fit to ARD
### Aliases: .format_survfit_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cardx:::.format_survfit_results(
  broom::tidy(survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE))
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-format_ttest_results")
### * dot-format_ttest_results

flush(stderr()); flush(stdout())

### Name: .format_ttest_results
### Title: Convert t-test to ARD
### Aliases: .format_ttest_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
cardx:::.format_ttest_results(
  by = "ARM",
  variable = "AGE",
  paired = FALSE,
  lst_tidy =
    cards::eval_capture_conditions(
      stats::t.test(ADSL[["AGE"]] ~ ADSL[["ARM"]], paired = FALSE) |>
        broom::tidy()
    )
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-format_wilcoxtest_results")
### * dot-format_wilcoxtest_results

flush(stderr()); flush(stdout())

### Name: .format_wilcoxtest_results
### Title: Convert Wilcoxon test to ARD
### Aliases: .format_wilcoxtest_results
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Pre-processing ADSL to have grouping factor (ARM here) with 2 levels
ADSL <- cards::ADSL |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  ard_stats_wilcox_test(by = "ARM", variables = "AGE")

cardx:::.format_wilcoxtest_results(
  by = "ARM",
  variable = "AGE",
  paired = FALSE,
  lst_tidy =
    cards::eval_capture_conditions(
      stats::wilcox.test(ADSL[["AGE"]] ~ ADSL[["ARM"]], paired = FALSE) |>
        broom::tidy()
    )
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-paired_data_pivot_wider")
### * dot-paired_data_pivot_wider

flush(stderr()); flush(stdout())

### Name: .paired_data_pivot_wider
### Title: Convert long paired data to wide
### Aliases: .paired_data_pivot_wider
### Keywords: internal

### ** Examples

cards::ADSL[c("ARM", "AGE")] |>
  dplyr::filter(ARM %in% c("Placebo", "Xanomeline High Dose")) |>
  dplyr::mutate(.by = ARM, USUBJID = dplyr::row_number()) |>
  dplyr::arrange(USUBJID, ARM) |>
  cardx:::.paired_data_pivot_wider(by = "ARM", variable = "AGE", id = "USUBJID")



cleanEx()
nameEx("dot-process_nested_list_as_df")
### * dot-process_nested_list_as_df

flush(stderr()); flush(stdout())

### Name: .process_nested_list_as_df
### Title: Convert Nested Lists to Column
### Aliases: .process_nested_list_as_df
### Keywords: internal

### ** Examples

ard <- ard_categorical(cards::ADSL, by = "ARM", variables = "AGEGR1")

cardx:::.process_nested_list_as_df(ard, NULL, "new_col")



cleanEx()
nameEx("dot-process_survfit_probs")
### * dot-process_survfit_probs

flush(stderr()); flush(stdout())

### Name: .process_survfit_probs
### Title: Process Survival Fit For Quantile Estimates
### Aliases: .process_survfit_probs
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "survival", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
  cardx:::.process_survfit_probs(probs = c(0.25, 0.75))
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-process_survfit_time")
### * dot-process_survfit_time

flush(stderr()); flush(stdout())

### Name: .process_survfit_time
### Title: Process Survival Fit For Time Estimates
### Aliases: .process_survfit_time
### Keywords: internal

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = c("survival", "broom"), reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
survival::survfit(survival::Surv(AVAL, CNSR) ~ TRTA, cards::ADTTE) |>
  cardx:::.process_survfit_time(times = c(60, 180), type = "risk")
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("dot-strata_normal_quantile")
### * dot-strata_normal_quantile

flush(stderr()); flush(stdout())

### Name: .strata_normal_quantile
### Title: Helper Function for the Estimation of Stratified Quantiles
### Aliases: .strata_normal_quantile
### Keywords: internal

### ** Examples

strata_data <- table(data.frame(
  "f1" = sample(c(TRUE, FALSE), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
))
ns <- colSums(strata_data)
ests <- strata_data["TRUE", ] / ns
vars <- ests * (1 - ests) / ns
weights <- rep(1 / length(ns), length(ns))

cardx:::.strata_normal_quantile(vars, weights, 0.95)



cleanEx()
nameEx("dot-unique_and_sorted")
### * dot-unique_and_sorted

flush(stderr()); flush(stdout())

### Name: .unique_and_sorted
### Title: ARD-flavor of unique()
### Aliases: .unique_and_sorted
### Keywords: internal

### ** Examples

cards:::.unique_and_sorted(factor(letters[c(5, 5:1)], levels = letters))

cards:::.unique_and_sorted(c(FALSE, TRUE, TRUE, FALSE))

cards:::.unique_and_sorted(c(5, 5:1))



cleanEx()
nameEx("dot-update_weights_strat_wilson")
### * dot-update_weights_strat_wilson

flush(stderr()); flush(stdout())

### Name: .update_weights_strat_wilson
### Title: Helper Function for the Estimation of Weights for
###   'proportion_ci_strat_wilson()'
### Aliases: .update_weights_strat_wilson
### Keywords: internal

### ** Examples

vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
sq <- 0.674
ws <- rep(1 / length(vs), length(vs))
ns <- c(22, 18, 17, 17, 14, 12)

cardx:::.update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)



cleanEx()
nameEx("proportion_ci")
### * proportion_ci

flush(stderr()); flush(stdout())

### Name: proportion_ci
### Title: Functions for Calculating Proportion Confidence Intervals
### Aliases: proportion_ci proportion_ci_wald proportion_ci_wilson
###   proportion_ci_clopper_pearson proportion_ci_agresti_coull
###   proportion_ci_jeffreys proportion_ci_strat_wilson is_binary

### ** Examples

## Don't show: 
if (do.call(asNamespace("cardx")$is_pkg_installed, list(pkg = "broom", reference_pkg = "cardx"))) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
x <- c(
  TRUE, TRUE, TRUE, TRUE, TRUE,
  FALSE, FALSE, FALSE, FALSE, FALSE
)

proportion_ci_wald(x, conf.level = 0.9)
proportion_ci_wilson(x, correct = TRUE)
proportion_ci_clopper_pearson(x)
proportion_ci_agresti_coull(x)
proportion_ci_jeffreys(x)
## Don't show: 
}) # examplesIf
## End(Don't show)
# Stratified Wilson confidence interval with unequal probabilities

set.seed(1)
rsp <- sample(c(TRUE, FALSE), 100, TRUE)
strata_data <- data.frame(
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
)
strata <- interaction(strata_data)
n_strata <- ncol(table(rsp, strata)) # Number of strata

proportion_ci_strat_wilson(
  x = rsp, strata = strata,
  conf.level = 0.90
)

# Not automatic setting of weights
proportion_ci_strat_wilson(
  x = rsp, strata = strata,
  weights = rep(1 / n_strata, n_strata),
  conf.level = 0.90
)




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
