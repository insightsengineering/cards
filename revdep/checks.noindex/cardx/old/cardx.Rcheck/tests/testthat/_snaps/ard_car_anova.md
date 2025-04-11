# ard_car_anova() works

    Code
      glm_ard_car_anova
    Message
      {cards} data frame: 6 x 8
    Output
           variable   context stat_name stat_label  stat fmt_fn
      1 factor(cyl) car_anova statistic  Statistic     0      1
      2 factor(cyl) car_anova        df  Degrees …     2      1
      3 factor(cyl) car_anova   p.value    p-value     1      1
      4  factor(am) car_anova statistic  Statistic     0      1
      5  factor(am) car_anova        df  Degrees …     1      1
      6  factor(am) car_anova   p.value    p-value 0.998      1
    Message
      i 2 more variables: warning, error

# ard_car_anova() messaging

    Code
      ard_car_anova(mtcars)
    Condition
      Error in `ard_car_anova()`:
      ! There was an error running `car::Anova()`. See error message below.
      x no applicable method for 'vcov' applied to an object of class "data.frame"

