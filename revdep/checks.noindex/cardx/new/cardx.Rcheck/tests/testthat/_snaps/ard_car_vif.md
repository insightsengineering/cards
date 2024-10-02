# ard_car_vif() works

    Code
      as.data.frame(ard_car_vif(lm(AGE ~ ARM + SEX, data = cards::ADSL)))
    Output
        variable context stat_name    stat_label     stat fmt_fn warning error
      1      ARM car_vif      GVIF          GVIF 1.015675      1    NULL  NULL
      2      ARM car_vif        df            df        2      1    NULL  NULL
      3      ARM car_vif     aGVIF Adjusted GVIF 1.003896      1    NULL  NULL
      4      SEX car_vif      GVIF          GVIF 1.015675      1    NULL  NULL
      5      SEX car_vif        df            df        1      1    NULL  NULL
      6      SEX car_vif     aGVIF Adjusted GVIF 1.007807      1    NULL  NULL

---

    Code
      as.data.frame(ard_car_vif(lm(AGE ~ BMIBL + EDUCLVL, data = cards::ADSL)))
    Output
        variable context stat_name stat_label     stat fmt_fn warning error
      1    BMIBL car_vif       VIF        VIF 1.010522      1    NULL  NULL
      2  EDUCLVL car_vif       VIF        VIF 1.010522      1    NULL  NULL

# ard_vif() issues friendly messaging for incorrect object passed in/can't get terms of model

    Code
      ard_car_vif(cards::ADSL)
    Condition
      Error in `ard_car_vif()`:
      ! There was an error running `car::vif()`. See below.
      x no applicable method for 'vcov' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

