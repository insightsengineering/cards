#' Check a package installation status or minimum required version
#'
#' @description
#' - `check_pkg_installed()`: checks whether a package is installed and
#' returns an error or `FALSE` if not available. If a package search is provided,
#' the function will check whether a minimum version of a package is required.
#'
#' - `get_pkg_dependencies()` returns a tibble with all
#' dependencies of a specific package.
#'
#' - `get_min_version_required()` will return, if any, the minimum version
#' of `pkg` required by `pkg_search`, `NULL` if no minimum version required.
#'
#' @param pkg (`string`)\cr
#'   Package required
#' @param n (`integer`)\cr
#'   Passed to `sys.call(n)`; the number of generations to go back. Default is `1L`.
#'   This is used to message user about the original function call the resulted
#'   in the prompt to install new packages.
#' @param pkg_search (`string`)\cr
#'   the package the function will search for a minimum required version from.
#' @param boolean `(logical(1))`
#'   logical indicating whether to return a `TRUE`/`FALSE`, rather
#'   than error when package/package version not available. Default is `FALSE`,
#'   which will return an error if `pkg` is not installed.
#' @param remove_duplicates `(logical(1))`\cr
#'   if several versions of a package are installed,
#'   should only the first one be returned?
#' @param lib.loc `(path)`
#'   location of `R` library trees to search through, see
#'   `utils::installed.packages()`.
#' @details
#' `get_all_packages_dependencies()` could be used to get the list of
#' dependencies of all installed packages.
#'
#' @return logical or error for `check_pkg_installed()`, `NULL` or character with
#' the minimum version required for `get_min_version_required()`, a tibble for
#' `get_pkg_dependencies()`.
#'
#' @name check_pkg_installed
#' @examplesIf interactive()
#' check_pkg_installed("broom", boolean = TRUE)
#' get_pkg_dependencies()
#' get_min_version_required("brms")
NULL

#' @rdname check_pkg_installed
#' @export
check_pkg_installed <- function(pkg,
                                pkg_search = "cards",
                                boolean = FALSE,
                                n = 1L) {
  # check if min version is required -------------------------------------------
  version <- get_min_version_required(pkg, pkg_search)
  compare <- attr(version, "compare")

  # get fn name from which the function was called -----------------------------
  fn <- tryCatch(paste0(as_label(sys.call(1)[[1]]), "()"), error = function(e) NULL)

  # check installation TRUE/FALSE ----------------------------------------------
  if (isTRUE(boolean)) {
    return(rlang::is_installed(pkg = pkg, version = version, compare = compare))
  }

  # prompt user to install package ---------------------------------------------
  rlang::check_installed(
    pkg = pkg,
    version = version,
    compare = compare,
    reason = switch(!is.null(fn), glue::glue("for `{fn}`")
    )
  )
  invisible()
}

#' @rdname check_pkg_installed
#' @export
get_pkg_dependencies <- function(pkg_search = "cards") {
  if (is.null(pkg_search)) {
    return(NULL)
  }
  description <- utils::packageDescription(pkg_search)
  if (identical(description, NA)) {
    return(NULL)
  }
  description |>
    unclass() |>
    dplyr::as_tibble() |>
    dplyr::select(
      any_of(c("Package", "Version", "Imports", "Depends",
               "Suggests", "Enhances", "LinkingTo"))
    ) |>
    dplyr::rename(
      pkg_search = "Package",
      pkg_search_version = "Version"
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("pkg_search", "pkg_search_version")),
      values_to = "pkg",
      names_to = "dependency_type",
    ) |>
    tidyr::separate_rows("pkg", sep = ",") |>
    dplyr::mutate(pkg = str_squish(.data$pkg)) |>
    dplyr::filter(!is.na(.data$pkg)) |>
    tidyr::separate(
      .data$pkg,
      into = c("pkg", "version"),
      sep = " ", extra = "merge", fill = "right"
    ) |>
    dplyr::mutate(
      compare = .data$version |> str_extract(pattern = "[>=<]+"),
      version = .data$version |> str_remove_all(pattern = "[\\(\\) >=<]")
    )
}

#' @rdname check_pkg_installed
#' @export
get_all_pkg_dependencies <- function(
    pkg_search = "cards",
    remove_duplicates = FALSE,
    lib.loc = NULL) {
  deps <-
    utils::installed.packages(lib.loc = lib.loc) |>
    dplyr::as_tibble() |>
    dplyr::select(
      any_of(c("Package", "Version", "LibPath", "Imports", "Depends",
               "Suggests", "Enhances", "LinkingTo"))
    ) |>
    dplyr::rename(
      pkg_search = "Package",
      pkg_search_version = "Version",
      lib_path = "LibPath"
    )

  if (!is.null(pkg_search)) {
    deps <- deps |> dplyr::filter(.data$pkg_search %in% .env$pkg_search)
  }
  if (remove_duplicates) {
    deps <- deps |> dplyr::distinct("pkg_search", .keep_all = TRUE)
  }

  deps |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("pkg_search", "pkg_search_version", "lib_path")),
      values_to = "pkg",
      names_to = "dependency_type",
    ) |>
    tidyr::separate_rows("pkg", sep = ",") |>
    dplyr::mutate(pkg = str_squish(.data$pkg)) |>
    dplyr::filter(!is.na(.data$pkg)) |>
    tidyr::separate(
      .data$pkg,
      into = c("pkg", "version"),
      sep = " ", extra = "merge", fill = "right"
    ) |>
    dplyr::mutate(
      compare = .data$version |> str_extract(pattern = "[>=<]+"),
      version = .data$version |> str_remove_all(pattern = "[\\(\\) >=<]")
    )
}

#' @rdname check_pkg_installed
#' @export
get_min_version_required <- function(pkg, pkg_search = "cards") {
  if (is.null(pkg_search)) {
    return(NULL)
  }
  res <- get_pkg_dependencies(pkg_search) |>
    dplyr::filter(.data$pkg == .env$pkg & !is.na(.data$version))
  if (nrow(res) == 0) {
    return(NULL)
  }
  version <- res[["version"]]
  attr(version, "compare") <- res[["compare"]]
  names(version) <- res[["dependency_type"]]
  version
}
