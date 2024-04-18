test_that("check_pkg_installed() works", {
  # dplyr will always be installed with cards
  expect_error(check_pkg_installed("dplyr"), NA)
  expect_true(is_pkg_installed("dplyr"))
  # recheck, but with two pkgs always installed
  expect_error(check_pkg_installed(c("dplyr", "tidyr")), NA)
  expect_true(is_pkg_installed(c("dplyr", "tidyr")))

  # check a package this does not exist
  expect_false(is_pkg_installed("dpl-eye-r"))
  # recheck with one pkg that is installed and another not
  expect_false(is_pkg_installed(c("dpl-eye-r", "tidyr")))

  expect_equal(
    get_min_version_required("tidyselect") |>
      dplyr::select(dependency_type, pkg, version, compare),
    dplyr::tibble(
      dependency_type = "Imports",
      pkg = "tidyselect",
      version = "1.2.0",
      compare = ">="
    )
  )
  expect_equal(
    get_min_version_required("brms", reference_pkg = NULL),
    dplyr::tibble(
      reference_pkg = NA_character_, reference_pkg_version = NA_character_,
      dependency_type = NA_character_, pkg = "brms", version = NA_character_,
      compare = NA_character_
    )
  )
  expect_equal(
    get_min_version_required("dplyr", reference_pkg = NULL),
    dplyr::tibble(
      reference_pkg = NA_character_, reference_pkg_version = NA_character_,
      dependency_type = NA_character_, pkg = "dplyr", version = NA_character_,
      compare = NA_character_
    )
  )

  expect_error(
    df_deps <- get_pkg_dependencies(),
    NA
  )

  expect_true(
    df_deps %>% inherits("data.frame")
  )

  expect_equal(
    names(df_deps),
    c("reference_pkg", "reference_pkg_version", "dependency_type", "pkg", "version", "compare")
  )

  skip_if(interactive())
  # expect an error msg for pkg that doesn't exist
  # note: if interactive(), user will be invited to install the missing pkg
  expect_snapshot(
    {
      user_facing_fn <- function() {
        set_cli_abort_call()
        check_pkg_installed(c("br000000m", "br1111111m"))
      }
      user_facing_fn()
    },
    error = TRUE
  )
  expect_error(
    check_pkg_installed("br000000m")
  )
  expect_error(
    check_pkg_installed("br000000m", fn = "test_fun()")
  )

  expect_equal(
    get_pkg_dependencies(NULL) |> nrow(),
    0L
  )
  expect_equal(
    get_pkg_dependencies("br000000m") |> nrow(),
    0L
  )
})
