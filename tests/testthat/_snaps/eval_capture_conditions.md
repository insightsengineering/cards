# eval_capture_conditions() works

    Code
      eval_capture_conditions(expr(TRUE))
    Output
      $result
      [1] TRUE
      
      $warning
      NULL
      
      $error
      NULL
      

---

    Code
      eval_capture_conditions(expr(cli::cli_abort("BIG ERROR")))
    Output
      $result
      NULL
      
      $warning
      NULL
      
      $error
      [1] "BIG ERROR"
      

---

    Code
      one_warn_foo <- (function() {
        cli::cli_warn("BIG WARNING")
        TRUE
      })
      eval_capture_conditions(expr(one_warn_foo()))
    Output
      $result
      [1] TRUE
      
      $warning
      [1] "BIG WARNING"
      
      $error
      NULL
      

---

    Code
      two_warn_foo <- (function() {
        cli::cli_warn("{.emph BIG} WARNING1")
        cli::cli_warn("{.emph BIG} WARNING2")
        TRUE
      })
      eval_capture_conditions(expr(two_warn_foo()))
    Output
      $result
      [1] TRUE
      
      $warning
      [1] "BIG WARNING1" "BIG WARNING2"
      
      $error
      NULL
      

