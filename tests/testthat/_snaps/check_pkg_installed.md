# check_pkg_installed() works

    Code
      user_facing_fn <- (function() {
        set_cli_abort_call()
        check_pkg_installed(c("br000000m", "br1111111m"))
      })
      user_facing_fn()
    Condition
      Error in `user_facing_fn()`:
      ! The packages "br000000m" and "br1111111m" are required.

