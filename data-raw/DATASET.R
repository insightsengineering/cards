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

ADSL |> mutate(TRTA = ARM)

usethis::use_data(ADSL, ADAE, ADTTE, overwrite = TRUE)
