test_that("check_pkg_installed() works", {
  # dplyr will always be installed with cards
  expect_error(
    check_pkg_installed("dplyr"),
    NA
  )
  expect_true(is_pkg_installed("dplyr"))

  expect_false(is_pkg_installed("dpl-eye-r"))

  mv <- c(Imports = "1.2.0")
  attr(mv, "compare") <- ">="
  expect_equal(
    get_min_version_required("tidyselect"),
    mv
  )
  expect_null(
    get_min_version_required("brms", reference_pkg = NULL)
  )
  expect_null(
    get_min_version_required("dplyr", reference_pkg = NULL)
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
  expect_error(
    check_pkg_installed("br000000m")
  )
  expect_error(
    check_pkg_installed("br000000m", fn = "test_fun()")
  )

  expect_equal(
    get_pkg_dependencies(NULL),
    NULL
  )
  expect_equal(
    get_pkg_dependencies("br000000m"),
    NULL
  )
})
