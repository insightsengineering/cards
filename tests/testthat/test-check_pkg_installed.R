test_that("check_pkg_installed() works", {
  # broom will always be installed with cards
  expect_error(
    check_pkg_installed("broom"),
    NA
  )
  expect_true(check_pkg_installed("broom", boolean = TRUE))

  expect_false(check_pkg_installed("br000000m", boolean = TRUE))

  mv <- c(Suggests = "1.0.5")
  attr(mv, "compare") <- ">="
  expect_equal(
    get_min_version_required("broom"),
    mv
  )
  expect_null(
    get_min_version_required("brms", pkg_search = NULL)
  )
  expect_null(
    get_min_version_required("broom", pkg_search = NULL)
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
    c("pkg_search", "pkg_search_version", "dependency_type", "pkg", "version", "compare")
  )

  expect_error(
    deps <- get_all_pkg_dependencies(),
    NA
  )
  expect_true(nrow(deps) > 1L)

  skip_if(interactive())
  # expect an error msg for pkg that doesn't exist
  # note: if interactive(), user will be invited to install the missing pkg
  expect_error(
    check_pkg_installed("br000000m")
  )
  expect_error(
    check_pkg_installed("br000000m", fn = "test_fun()")
  )
})
