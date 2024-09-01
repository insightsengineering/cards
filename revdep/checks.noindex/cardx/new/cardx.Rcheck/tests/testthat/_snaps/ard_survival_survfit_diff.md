# ard_survival_survfit_diff() messaging

    Code
      ard_survival_survfit_diff(survfit(Surv(AVAL, 1 - CNSR) ~ SEX + TRTA, cards::ADTTE),
      times = c(25, 50))
    Condition
      Error in `ard_survival_survfit_diff()`:
      ! The <survfit> object passed in argument `x` must be stratified by a single variable.

---

    Code
      ard_survival_survfit_diff(survfit(Surv(AVAL, 1 - CNSR) ~ constant, dplyr::mutate(
        cards::ADTTE, constant = 1L)), times = c(25, 50))
    Condition
      Error in `ard_survival_survfit_diff()`:
      ! The <survfit> object's stratifying variable must have 2 or more levels.

---

    Code
      ard_survival_survfit_diff(survfit(coxph(Surv(AVAL, CNSR) ~ SEX + strata(TRTA),
      cards::ADTTE)), times = c(25, 50))
    Condition
      Error in `ard_survival_survfit_diff()`:
      ! Argument `x` cannot be class <survfitms/survfitcox>.

