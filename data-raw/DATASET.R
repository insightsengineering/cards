## code to prepare `DATASET` dataset goes here

ADSL <- haven::read_xpt(
  "https://github.com/cdisc-org/sdtm-adam-pilot-project/raw/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt"
)
ADAE <- haven::read_xpt(
  "https://github.com/cdisc-org/sdtm-adam-pilot-project/raw/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adae.xpt"
)
ADTTE <- haven::read_xpt(
  "https://github.com/cdisc-org/sdtm-adam-pilot-project/raw/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adtte.xpt"
)

ADSL$TRTA <- ADSL$TRT01A
labelled::var_label(ADSL$TRTA) <- labelled::var_label(ADAE$TRTA)

ADAE <-
  withr::with_seed(
    seed = 1L,
    ADAE |>
      dplyr::rowwise() |>
      dplyr::mutate(
        .after = "AESEV",
        AETOXGR = dplyr::case_when(
          AESDTH == "Y" ~ 5,
          AESEV == "SEVERE" ~ 4,
          AESEV == "MODERATE" ~ sample(2:3, 1),
          AESEV == "MILD" ~ 1,
        ) |> factor(levels = 1:5)
      ) |>
      dplyr::ungroup()
  ) |>
  labelled::set_variable_labels(AETOXGR = "Toxicity Grade")

usethis::use_data(ADSL, ADAE, ADTTE, overwrite = TRUE)
