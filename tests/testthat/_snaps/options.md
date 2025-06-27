# options(cards.round_type) messaging

    Code
      withr::with_options(list(cards.round_type = "NOT-CORRECT"), ard_categorical(
        data.frame(x = c(T, F)), variables = everything(), statistic = ~"p"))
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `fmt_fun = pmap(...)`.
      Caused by error in `ard_categorical()`:
      ! The `cards.round_type` option must be one of "round-half-up" and "round-to-even".

