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
      
      attr(,"class")
      [1] "captured_condition" "list"              

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
      
      attr(,"class")
      [1] "captured_condition" "list"              

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
      
      attr(,"class")
      [1] "captured_condition" "list"              

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
      
      attr(,"class")
      [1] "captured_condition" "list"              

# captured_condition_as_message() works

    Code
      captured_condition_as_message(eval_capture_conditions(stop(
        "This is an {error}!")))
    Message
      The following error occured:
      x This is an {error}!
    Output
      NULL

---

    Code
      captured_condition_as_message(eval_capture_conditions({
        warning("This is a {warning} 1")
        warning("This is a {warning} 2")
        NULL
      }), type = "warning")
    Message
      The following warning occured:
      x This is a {warning} 1 and This is a {warning} 2
    Output
      NULL

# captured_condition_as_error() works

    Code
      captured_condition_as_error(eval_capture_conditions(stop("This is an {error}!")))
    Condition
      Error in `captured_condition_as_error()`:
      ! The following error occured:
      x This is an {error}!

---

    Code
      captured_condition_as_error(eval_capture_conditions({
        warning("This is a {warning} 1")
        warning("This is a {warning} 2")
        NULL
      }), type = "warning")
    Condition
      Error in `captured_condition_as_error()`:
      ! The following warning occured:
      x This is a {warning} 1 and This is a {warning} 2

