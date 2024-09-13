# construct_model() works

    Code
      dplyr::filter(as.data.frame(ard_regression(construct_model(data = dplyr::rename(
        mtcars, `M P G` = mpg), formula = reformulate2(c("M P G", "cyl"), response = "hp"),
      method = "lm"))), stat_name %in% c("term", "estimate", "p.value"))
    Output
        variable    context stat_name  stat_label        stat fmt_fn
      1    M P G regression      term        term     `M P G`   NULL
      2    M P G regression  estimate Coefficient   -2.774769      1
      3    M P G regression   p.value     p-value   0.2125285      1
      4      cyl regression      term        term         cyl   NULL
      5      cyl regression  estimate Coefficient    23.97863      1
      6      cyl regression   p.value     p-value 0.002814958      1

# construct_model() messaging

    Code
      construct_model(data = mtcars, method = "survival::coxph", formula = survival::Surv(
        mpg, am) ~ cyl)
    Condition
      Error in `construct_model()`:
      ! Argument `method` cannot be namespaced when passed as a <string>.

---

    Code
      construct_model(data = mtcars, method = letters, formula = am ~ cyl)
    Condition
      Error in `construct_model()`:
      ! Argument `method` must be a <string> or <function>.

---

    Code
      construct_model(data = mtcars, method = "glm", formula = am ~ cyl, method.args = list(
        iamnotavalidparameter = "binomial"))
    Condition
      Error in `construct_model()`:
      ! There was an error evaluating the model `glm(formula = am ~ cyl, data = ., iamnotavalidparameter = "binomial")`
      Caused by error in `glm.control()`:
      ! unused argument (iamnotavalidparameter = "binomial")

---

    Code
      construct_model(data = mtcars, method = glm, formula = am ~ cyl, method.args = list(
        iamnotavalidparameter = "binomial"))
    Condition
      Error in `construct_model()`:
      ! There was an error evaluating the model
      Caused by error in `glm.control()`:
      ! unused argument (iamnotavalidparameter = "binomial")

---

    Code
      data(api, package = "survey")
      design <- survey::svydesign(id = ~1, weights = ~pw, data = apistrat)
      construct_model(data = design, formula = api00 ~ api99, method = "svyglm",
      method.args = list(iamnotavalidparameter = stats::gaussian()), package = "survey")
    Condition
      Error in `construct_model()`:
      ! There was an error evaluating the model `svyglm(formula = api00 ~ api99, design = ., iamnotavalidparameter = stats::gaussian())`
      Caused by error in `glm.control()`:
      ! unused argument (iamnotavalidparameter = list("gaussian", "identity", function (mu) 
      mu, function (eta) 
      eta, function (mu) 
      rep.int(1, length(mu)), function (y, mu, wt) 
      wt * ((y - mu)^2), function (y, n, mu, wt, dev) 
      {
          nobs <- length(y)
          nobs * (log(dev/nobs * 2 * pi) + 1) + 2 - sum(log(wt))
      }, function (eta) 
      rep.int(1, length(eta)), expression({
          n <- rep.int(1, nobs)
          if (is.null(etastart) && is.null(start) && is.null(mustart) && ((family$link == "inverse" && any(y == 0)) || (family$link == "log" && any(y <= 0)))) stop("cannot find valid starting values: please specify some")
          mustart <- y
      }), function (mu) 
      TRUE, function (eta) 
      TRUE, NA))

---

    Code
      data(api, package = "survey")
      design <- survey::svydesign(id = ~1, weights = ~pw, data = apistrat)
      construct_model(data = design, formula = api00 ~ api99, method = "svyglm",
      method.args = list(iamnotavalidparameter = stats::gaussian()))
    Condition
      Error in `construct_model()`:
      ! There was an error evaluating the model `svyglm(formula = api00 ~ api99, design = ., iamnotavalidparameter = stats::gaussian())`
      Caused by error in `glm.control()`:
      ! unused argument (iamnotavalidparameter = list("gaussian", "identity", function (mu) 
      mu, function (eta) 
      eta, function (mu) 
      rep.int(1, length(mu)), function (y, mu, wt) 
      wt * ((y - mu)^2), function (y, n, mu, wt, dev) 
      {
          nobs <- length(y)
          nobs * (log(dev/nobs * 2 * pi) + 1) + 2 - sum(log(wt))
      }, function (eta) 
      rep.int(1, length(eta)), expression({
          n <- rep.int(1, nobs)
          if (is.null(etastart) && is.null(start) && is.null(mustart) && ((family$link == "inverse" && any(y == 0)) || (family$link == "log" && any(y <= 0)))) stop("cannot find valid starting values: please specify some")
          mustart <- y
      }), function (mu) 
      TRUE, function (eta) 
      TRUE, NA))

