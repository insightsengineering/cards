pkgname <- "broom.helpers"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('broom.helpers')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("assert_package")
### * assert_package

flush(stderr()); flush(stdout())

### Name: assert_package
### Title: Check a package installation status or minimum required version
### Aliases: assert_package .assert_package .get_package_dependencies
###   .get_all_packages_dependencies .get_min_version_required

### ** Examples




cleanEx()
nameEx("model_compute_terms_contributions")
### * model_compute_terms_contributions

flush(stderr()); flush(stdout())

### Name: model_compute_terms_contributions
### Title: Compute a matrix of terms contributions
### Aliases: model_compute_terms_contributions
###   model_compute_terms_contributions.default

### ** Examples




cleanEx()
nameEx("model_get_assign")
### * model_get_assign

flush(stderr()); flush(stdout())

### Name: model_get_assign
### Title: Get the assign attribute of model matrix of a model
### Aliases: model_get_assign model_get_assign.default
###   model_get_assign.vglm model_get_assign.model_fit

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_assign()



cleanEx()
nameEx("model_get_coefficients_type")
### * model_get_coefficients_type

flush(stderr()); flush(stdout())

### Name: model_get_coefficients_type
### Title: Get coefficient type
### Aliases: model_get_coefficients_type
###   model_get_coefficients_type.default model_get_coefficients_type.glm
###   model_get_coefficients_type.negbin model_get_coefficients_type.geeglm
###   model_get_coefficients_type.fixest model_get_coefficients_type.biglm
###   model_get_coefficients_type.glmerMod
###   model_get_coefficients_type.clogit model_get_coefficients_type.polr
###   model_get_coefficients_type.multinom
###   model_get_coefficients_type.svyolr model_get_coefficients_type.clm
###   model_get_coefficients_type.clmm model_get_coefficients_type.coxph
###   model_get_coefficients_type.crr model_get_coefficients_type.tidycrr
###   model_get_coefficients_type.cch model_get_coefficients_type.model_fit
###   model_get_coefficients_type.LORgee model_get_coefficients_type.vglm
###   model_get_coefficients_type.vgam

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_coefficients_type()

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
glm(Survived ~ Class + Age * Sex, data = df, weights = df$n, family = binomial) |>
  model_get_coefficients_type()



cleanEx()
nameEx("model_get_contrasts")
### * model_get_contrasts

flush(stderr()); flush(stdout())

### Name: model_get_contrasts
### Title: Get contrasts used in the model
### Aliases: model_get_contrasts model_get_contrasts.model_fit
###   model_get_contrasts.zeroinfl model_get_contrasts.hurdle
###   model_get_contrasts.betareg

### ** Examples

glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) |>
  model_get_contrasts()



cleanEx()
nameEx("model_get_model")
### * model_get_model

flush(stderr()); flush(stdout())

### Name: model_get_model
### Title: Get the model from model objects
### Aliases: model_get_model model_get_model.default model_get_model.mira

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_model()



cleanEx()
nameEx("model_get_model_frame")
### * model_get_model_frame

flush(stderr()); flush(stdout())

### Name: model_get_model_frame
### Title: Get the model frame of a model
### Aliases: model_get_model_frame model_get_model_frame.default
###   model_get_model_frame.coxph model_get_model_frame.svycoxph
###   model_get_model_frame.survreg model_get_model_frame.biglm
###   model_get_model_frame.model_fit model_get_model_frame.fixest

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_model_frame() |>
  head()



cleanEx()
nameEx("model_get_model_matrix")
### * model_get_model_matrix

flush(stderr()); flush(stdout())

### Name: model_get_model_matrix
### Title: Get the model matrix of a model
### Aliases: model_get_model_matrix model_get_model_matrix.default
###   model_get_model_matrix.multinom model_get_model_matrix.clm
###   model_get_model_matrix.brmsfit model_get_model_matrix.glmmTMB
###   model_get_model_matrix.plm model_get_model_matrix.biglm
###   model_get_model_matrix.model_fit model_get_model_matrix.fixest
###   model_get_model_matrix.LORgee model_get_model_matrix.betareg
###   model_get_model_matrix.cch model_get_model_matrix.vglm
###   model_get_model_matrix.vgam

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_model_matrix() |>
  head()



cleanEx()
nameEx("model_get_n")
### * model_get_n

flush(stderr()); flush(stdout())

### Name: model_get_n
### Title: Get the number of observations
### Aliases: model_get_n model_get_n.default model_get_n.glm
###   model_get_n.glmerMod model_get_n.multinom model_get_n.LORgee
###   model_get_n.coxph model_get_n.survreg model_get_n.model_fit
###   model_get_n.tidycrr

### ** Examples

lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
  model_get_n()

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial,
  contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
)
mod |> model_get_n()

## Not run: 
##D mod <- glm(
##D   Survived ~ Class * Age + Sex,
##D   data = Titanic |> as.data.frame(),
##D   weights = Freq, family = binomial
##D )
##D mod |> model_get_n()
##D 
##D d <- dplyr::as_tibble(Titanic) |>
##D   dplyr::group_by(Class, Sex, Age) |>
##D   dplyr::summarise(
##D     n_survived = sum(n * (Survived == "Yes")),
##D     n_dead = sum(n * (Survived == "No"))
##D   )
##D mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
##D mod |> model_get_n()
##D 
##D mod <- glm(response ~ age + grade * trt, gtsummary::trial, family = poisson)
##D mod |> model_get_n()
##D 
##D mod <- glm(
##D   response ~ trt * grade + offset(ttdeath),
##D   gtsummary::trial,
##D   family = poisson
##D )
##D mod |> model_get_n()
##D 
##D dont
##D df <- survival::lung |> dplyr::mutate(sex = factor(sex))
##D mod <- survival::coxph(survival::Surv(time, status) ~ ph.ecog + age + sex, data = df)
##D mod |> model_get_n()
##D 
##D mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
##D mod |> model_get_n()
##D 
##D mod <- lme4::glmer(response ~ trt * grade + (1 | stage),
##D   family = binomial, data = gtsummary::trial
##D )
##D mod |> model_get_n()
##D 
##D mod <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
##D   family = binomial, data = lme4::cbpp
##D )
##D mod |> model_get_n()
## End(Not run)



cleanEx()
nameEx("model_get_nlevels")
### * model_get_nlevels

flush(stderr()); flush(stdout())

### Name: model_get_nlevels
### Title: Get the number of levels for each factor used in 'xlevels'
### Aliases: model_get_nlevels model_get_nlevels.default

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_nlevels()



cleanEx()
nameEx("model_get_offset")
### * model_get_offset

flush(stderr()); flush(stdout())

### Name: model_get_offset
### Title: Get model offset
### Aliases: model_get_offset model_get_offset.default

### ** Examples

mod <- glm(
  response ~ trt + offset(log(ttdeath)),
  gtsummary::trial,
  family = poisson
)
mod |> model_get_offset()



cleanEx()
nameEx("model_get_pairwise_contrasts")
### * model_get_pairwise_contrasts

flush(stderr()); flush(stdout())

### Name: model_get_pairwise_contrasts
### Title: Get pairwise comparison of the levels of a categorical variable
### Aliases: model_get_pairwise_contrasts

### ** Examples

## Don't show: 
if (.assert_package("emmeans", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("model_get_response")
### * model_get_response

flush(stderr()); flush(stdout())

### Name: model_get_response
### Title: Get model response
### Aliases: model_get_response model_get_response.default
###   model_get_response.glm model_get_response.glmerMod
###   model_get_response.model_fit

### ** Examples

lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
  model_get_response()

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial,
  contrasts = list(stage = contr.sum, grade = contr.treatment(3, 2), trt = "contr.SAS")
)
mod |> model_get_response()

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq,
  family = binomial
)
mod |> model_get_response()

d <- dplyr::as_tibble(Titanic) |>
  dplyr::group_by(Class, Sex, Age) |>
  dplyr::summarise(
    n_survived = sum(n * (Survived == "Yes")),
    n_dead = sum(n * (Survived == "No"))
  )
mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial, y = FALSE)
mod |> model_get_response()



cleanEx()
nameEx("model_get_response_variable")
### * model_get_response_variable

flush(stderr()); flush(stdout())

### Name: model_get_response_variable
### Title: Get the name of the response variable
### Aliases: model_get_response_variable
###   model_get_response_variable.default

### ** Examples

lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
  model_get_response_variable()

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial
)
mod |> model_get_response_variable()

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq,
  family = binomial
)
mod |> model_get_response_variable()



cleanEx()
nameEx("model_get_terms")
### * model_get_terms

flush(stderr()); flush(stdout())

### Name: model_get_terms
### Title: Get the terms of a model
### Aliases: model_get_terms model_get_terms.default
###   model_get_terms.brmsfit model_get_terms.glmmTMB
###   model_get_terms.model_fit model_get_terms.betareg model_get_terms.cch
###   model_get_terms.fixest

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_terms()



cleanEx()
nameEx("model_get_weights")
### * model_get_weights

flush(stderr()); flush(stdout())

### Name: model_get_weights
### Title: Get sampling weights used by a model
### Aliases: model_get_weights model_get_weights.default
###   model_get_weights.svyglm model_get_weights.svrepglm
###   model_get_weights.model_fit

### ** Examples

mod <- lm(Sepal.Length ~ Sepal.Width, iris)
mod |> model_get_weights()

mod <- lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars, weights = mtcars$gear)
mod |> model_get_weights()

mod <- glm(
  response ~ stage * grade + trt,
  gtsummary::trial,
  family = binomial
)
mod |> model_get_weights()

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq,
  family = binomial
)
mod |> model_get_weights()

d <- dplyr::as_tibble(Titanic) |>
  dplyr::group_by(Class, Sex, Age) |>
  dplyr::summarise(
    n_survived = sum(n * (Survived == "Yes")),
    n_dead = sum(n * (Survived == "No"))
  )
mod <- glm(cbind(n_survived, n_dead) ~ Class * Age + Sex, data = d, family = binomial)
mod |> model_get_weights()



cleanEx()
nameEx("model_get_xlevels")
### * model_get_xlevels

flush(stderr()); flush(stdout())

### Name: model_get_xlevels
### Title: Get xlevels used in the model
### Aliases: model_get_xlevels model_get_xlevels.default
###   model_get_xlevels.lmerMod model_get_xlevels.glmerMod
###   model_get_xlevels.felm model_get_xlevels.brmsfit
###   model_get_xlevels.glmmTMB model_get_xlevels.plm
###   model_get_xlevels.model_fit

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_xlevels()



cleanEx()
nameEx("model_identify_variables")
### * model_identify_variables

flush(stderr()); flush(stdout())

### Name: model_identify_variables
### Title: Identify for each coefficient of a model the corresponding
###   variable
### Aliases: model_identify_variables model_identify_variables.default
###   model_identify_variables.lavaan model_identify_variables.aov
###   model_identify_variables.clm model_identify_variables.clmm
###   model_identify_variables.gam model_identify_variables.model_fit
###   model_identify_variables.logitr

### ** Examples

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
glm(
  Survived ~ Class + Age * Sex,
  data = df, weights = df$n,
  family = binomial
) |>
  model_identify_variables()

lm(
  Sepal.Length ~ poly(Sepal.Width, 2) + Species,
  data = iris,
  contrasts = list(Species = contr.sum)
) |>
  model_identify_variables()



cleanEx()
nameEx("model_list_contrasts")
### * model_list_contrasts

flush(stderr()); flush(stdout())

### Name: model_list_contrasts
### Title: List contrasts used by a model
### Aliases: model_list_contrasts model_list_contrasts.default

### ** Examples

glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) |>
  model_list_contrasts()



cleanEx()
nameEx("model_list_higher_order_variables")
### * model_list_higher_order_variables

flush(stderr()); flush(stdout())

### Name: model_list_higher_order_variables
### Title: List higher order variables of a model
### Aliases: model_list_higher_order_variables
###   model_list_higher_order_variables.default

### ** Examples

lm(hp ~ mpg + factor(cyl) + disp:hp, mtcars) |>
  model_list_higher_order_variables()

mod <- glm(
  response ~ stage * grade + trt:stage,
  gtsummary::trial,
  family = binomial
)
mod |> model_list_higher_order_variables()

mod <- glm(
  Survived ~ Class * Age + Sex,
  data = Titanic |> as.data.frame(),
  weights = Freq,
  family = binomial
)
mod |> model_list_higher_order_variables()



cleanEx()
nameEx("model_list_terms_levels")
### * model_list_terms_levels

flush(stderr()); flush(stdout())

### Name: model_list_terms_levels
### Title: List levels of categorical terms
### Aliases: model_list_terms_levels model_list_terms_levels.default

### ** Examples

glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) |>
  model_list_terms_levels()

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
)
mod |> model_list_terms_levels()
mod |> model_list_terms_levels("{level} vs {reference_level}")
mod |> model_list_terms_levels("{variable} [{level} - {reference_level}]")
mod |> model_list_terms_levels(
  "{ifelse(reference, level, paste(level, '-', reference_level))}"
)



cleanEx()
nameEx("model_list_variables")
### * model_list_variables

flush(stderr()); flush(stdout())

### Name: model_list_variables
### Title: List all the variables used in a model
### Aliases: model_list_variables model_list_variables.default
###   model_list_variables.lavaan model_list_variables.logitr

### ** Examples

## Don't show: 
if (.assert_package("gtsummary", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("scope_tidy")
### * scope_tidy

flush(stderr()); flush(stdout())

### Name: scope_tidy
### Title: Scoping a tidy tibble allowing to tidy select
### Aliases: scope_tidy

### ** Examples

mod <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
tt <- mod |> tidy_and_attach() |> tidy_add_contrasts()

scope_tidy(tt) |> str()
scope_tidy(tt, data = model_get_model_frame(mod)) |> str()

scope_tidy(tt) |> dplyr::select(dplyr::starts_with("Se")) |> names()
scope_tidy(tt) |> dplyr::select(where(is.factor)) |> names()
scope_tidy(tt) |> dplyr::select(all_continuous()) |> names()
scope_tidy(tt) |> dplyr::select(all_contrasts()) |> names()
scope_tidy(tt) |> dplyr::select(all_interaction()) |> names()
scope_tidy(tt) |> dplyr::select(all_intercepts()) |> names()



cleanEx()
nameEx("select_helpers")
### * select_helpers

flush(stderr()); flush(stdout())

### Name: select_helpers
### Title: Select helper functions
### Aliases: select_helpers all_continuous all_categorical all_dichotomous
###   all_interaction all_ran_pars all_ran_vals all_intercepts
###   all_contrasts

### ** Examples

## Don't show: 
if (.assert_package("emmeans", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("seq_range")
### * seq_range

flush(stderr()); flush(stdout())

### Name: seq_range
### Title: Sequence generation between min and max
### Aliases: seq_range

### ** Examples

seq_range(iris$Petal.Length)



cleanEx()
nameEx("tidy_add_coefficients_type")
### * tidy_add_coefficients_type

flush(stderr()); flush(stdout())

### Name: tidy_add_coefficients_type
### Title: Add coefficients type and label as attributes
### Aliases: tidy_add_coefficients_type

### ** Examples

ex1 <- lm(hp ~ mpg + factor(cyl), mtcars) |>
  tidy_and_attach() |>
  tidy_add_coefficients_type()
attr(ex1, "coefficients_type")
attr(ex1, "coefficients_label")

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
ex2 <- glm(
  Survived ~ Class + Age * Sex,
  data = df,
  weights = df$n,
  family = binomial
) |>
  tidy_and_attach(exponentiate = TRUE) |>
  tidy_add_coefficients_type()
attr(ex2, "coefficients_type")
attr(ex2, "coefficients_label")



cleanEx()
nameEx("tidy_add_contrasts")
### * tidy_add_contrasts

flush(stderr()); flush(stdout())

### Name: tidy_add_contrasts
### Title: Add contrasts type for categorical variables
### Aliases: tidy_add_contrasts

### ** Examples

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

glm(
  Survived ~ Class + Age + Sex,
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
) |>
  tidy_and_attach() |>
  tidy_add_contrasts()



cleanEx()
nameEx("tidy_add_estimate_to_reference_rows")
### * tidy_add_estimate_to_reference_rows

flush(stderr()); flush(stdout())

### Name: tidy_add_estimate_to_reference_rows
### Title: Add an estimate value to references rows for categorical
###   variables
### Aliases: tidy_add_estimate_to_reference_rows

### ** Examples

## Don't show: 
if (require("gtsummary") && require("emmeans")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_add_header_rows")
### * tidy_add_header_rows

flush(stderr()); flush(stdout())

### Name: tidy_add_header_rows
### Title: Add header rows variables with several terms
### Aliases: tidy_add_header_rows

### ** Examples

## Don't show: 
if (.assert_package("gtsummary", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_add_n")
### * tidy_add_n

flush(stderr()); flush(stdout())

### Name: tidy_add_n
### Title: Add the (weighted) number of observations
### Aliases: tidy_add_n

### ** Examples




cleanEx()
nameEx("tidy_add_pairwise_contrasts")
### * tidy_add_pairwise_contrasts

flush(stderr()); flush(stdout())

### Name: tidy_add_pairwise_contrasts
### Title: Add pairwise contrasts for categorical variables
### Aliases: tidy_add_pairwise_contrasts

### ** Examples

## Don't show: 
if (.assert_package("emmeans", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_add_reference_rows")
### * tidy_add_reference_rows

flush(stderr()); flush(stdout())

### Name: tidy_add_reference_rows
### Title: Add references rows for categorical variables
### Aliases: tidy_add_reference_rows

### ** Examples

## Don't show: 
if (.assert_package("gtsummary", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_add_term_labels")
### * tidy_add_term_labels

flush(stderr()); flush(stdout())

### Name: tidy_add_term_labels
### Title: Add term labels
### Aliases: tidy_add_term_labels

### ** Examples




cleanEx()
nameEx("tidy_add_variable_labels")
### * tidy_add_variable_labels

flush(stderr()); flush(stdout())

### Name: tidy_add_variable_labels
### Title: Add variable labels
### Aliases: tidy_add_variable_labels

### ** Examples

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) |>
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Sex"
  )

glm(Survived ~ Class * Age * Sex, data = df, weights = df$n, family = binomial) |>
  tidy_and_attach() |>
  tidy_add_variable_labels(
    labels = list(
      "(Intercept)" ~ "Custom intercept",
      Sex ~ "Gender",
      "Class:Age" ~ "Custom label"
    )
  )



cleanEx()
nameEx("tidy_all_effects")
### * tidy_all_effects

flush(stderr()); flush(stdout())

### Name: tidy_all_effects
### Title: Marginal Predictions at the mean with 'effects::allEffects()'
### Aliases: tidy_all_effects

### ** Examples

## Don't show: 
if (.assert_package("effects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_attach_model")
### * tidy_attach_model

flush(stderr()); flush(stdout())

### Name: tidy_attach_model
### Title: Attach a full model to the tibble of model terms
### Aliases: tidy_attach_model tidy_and_attach tidy_get_model
###   tidy_detach_model

### ** Examples

mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
tt <- mod |>
  tidy_and_attach(conf.int = TRUE)
tt
tidy_get_model(tt)



cleanEx()
nameEx("tidy_avg_comparisons")
### * tidy_avg_comparisons

flush(stderr()); flush(stdout())

### Name: tidy_avg_comparisons
### Title: Marginal Contrasts with 'marginaleffects::avg_comparisons()'
### Aliases: tidy_avg_comparisons

### ** Examples

## Don't show: 
if (.assert_package("marginaleffects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_avg_slopes")
### * tidy_avg_slopes

flush(stderr()); flush(stdout())

### Name: tidy_avg_slopes
### Title: Marginal Slopes / Effects with 'marginaleffects::avg_slopes()'
### Aliases: tidy_avg_slopes

### ** Examples

## Don't show: 
if (.assert_package("marginaleffects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_disambiguate_terms")
### * tidy_disambiguate_terms

flush(stderr()); flush(stdout())

### Name: tidy_disambiguate_terms
### Title: Disambiguate terms
### Aliases: tidy_disambiguate_terms

### ** Examples




cleanEx()
nameEx("tidy_ggpredict")
### * tidy_ggpredict

flush(stderr()); flush(stdout())

### Name: tidy_ggpredict
### Title: Marginal Predictions with 'ggeffects::ggpredict()'
### Aliases: tidy_ggpredict

### ** Examples

## Don't show: 
if (.assert_package("ggeffects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_group_by")
### * tidy_group_by

flush(stderr()); flush(stdout())

### Name: tidy_group_by
### Title: Group results by selected columns
### Aliases: tidy_group_by auto_group_by

### ** Examples

## Don't show: 
if (require("nnet")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
mod <- multinom(Species ~ Petal.Width + Petal.Length, data = iris)
mod |> tidy_and_attach() |> tidy_group_by()

mod |>
  tidy_and_attach() |>
  tidy_group_by(group_labels = c(versicolor = "harlequin blueflag"))

mod |> tidy_and_attach() |> tidy_group_by(group_by = NULL)

mod |>
  tidy_and_attach() |>
  tidy_identify_variables() |>
  tidy_group_by(group_by = variable)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_identify_variables")
### * tidy_identify_variables

flush(stderr()); flush(stdout())

### Name: tidy_identify_variables
### Title: Identify the variable corresponding to each model coefficient
### Aliases: tidy_identify_variables

### ** Examples

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
glm(
  Survived ~ Class + Age * Sex,
  data = df,
  weights = df$n,
  family = binomial
) |>
  tidy_and_attach() |>
  tidy_identify_variables()

lm(
  Sepal.Length ~ poly(Sepal.Width, 2) + Species,
  data = iris,
  contrasts = list(Species = contr.sum)
) |>
  tidy_and_attach(conf.int = TRUE) |>
  tidy_identify_variables()



cleanEx()
nameEx("tidy_marginal_contrasts")
### * tidy_marginal_contrasts

flush(stderr()); flush(stdout())

### Name: tidy_marginal_contrasts
### Title: Marginal Contrasts with 'marginaleffects::avg_comparisons()'
### Aliases: tidy_marginal_contrasts variables_to_contrast

### ** Examples

## Don't show: 
if (.assert_package("marginaleffects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_marginal_predictions")
### * tidy_marginal_predictions

flush(stderr()); flush(stdout())

### Name: tidy_marginal_predictions
### Title: Marginal Predictions with 'marginaleffects::avg_predictions()'
### Aliases: tidy_marginal_predictions variables_to_predict
###   plot_marginal_predictions

### ** Examples

## Don't show: 
if (.assert_package("marginaleffects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# example code

## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_margins")
### * tidy_margins

flush(stderr()); flush(stdout())

### Name: tidy_margins
### Title: Average Marginal Effects with 'margins::margins()'
### Aliases: tidy_margins

### ** Examples

## Don't show: 
if (.assert_package("margins", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_multgee")
### * tidy_multgee

flush(stderr()); flush(stdout())

### Name: tidy_multgee
### Title: Tidy a 'multgee' model
### Aliases: tidy_multgee

### ** Examples

## Don't show: 
if (.assert_package("multgee", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_parameters")
### * tidy_parameters

flush(stderr()); flush(stdout())

### Name: tidy_parameters
### Title: Tidy a model with parameters package
### Aliases: tidy_parameters

### ** Examples

## Don't show: 
if (.assert_package("parameters", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_plus_plus")
### * tidy_plus_plus

flush(stderr()); flush(stdout())

### Name: tidy_plus_plus
### Title: Tidy a model and compute additional informations
### Aliases: tidy_plus_plus

### ** Examples

## Don't show: 
if (require("gtsummary") && require("emmeans")) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_remove_intercept")
### * tidy_remove_intercept

flush(stderr()); flush(stdout())

### Name: tidy_remove_intercept
### Title: Remove intercept(s)
### Aliases: tidy_remove_intercept

### ** Examples

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived))
glm(Survived ~ Class + Age + Sex, data = df, weights = df$n, family = binomial) |>
  tidy_and_attach() |>
  tidy_remove_intercept()



cleanEx()
nameEx("tidy_select_variables")
### * tidy_select_variables

flush(stderr()); flush(stdout())

### Name: tidy_select_variables
### Title: Select variables to keep/drop
### Aliases: tidy_select_variables

### ** Examples

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived))
res <-
  glm(Survived ~ Class + Age * Sex, data = df, weights = df$n, family = binomial) |>
  tidy_and_attach() |>
  tidy_identify_variables()

res
res |> tidy_select_variables()
res |> tidy_select_variables(include = "Class")
res |> tidy_select_variables(include = -c("Age", "Sex"))
res |> tidy_select_variables(include = starts_with("A"))
res |> tidy_select_variables(include = all_categorical())
res |> tidy_select_variables(include = all_dichotomous())
res |> tidy_select_variables(include = all_interaction())
res |> tidy_select_variables(
  include = c("Age", all_categorical(dichotomous = FALSE), all_interaction())
)



cleanEx()
nameEx("tidy_vgam")
### * tidy_vgam

flush(stderr()); flush(stdout())

### Name: tidy_vgam
### Title: Tidy a 'vglm' or a 'vgam' model
### Aliases: tidy_vgam

### ** Examples

## Don't show: 
if (.assert_package("VGAM", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("tidy_zeroinfl")
### * tidy_zeroinfl

flush(stderr()); flush(stdout())

### Name: tidy_zeroinfl
### Title: Tidy a 'zeroinfl' or a 'hurdle' model
### Aliases: tidy_zeroinfl

### ** Examples

## Don't show: 
if (.assert_package("pscl", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



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
