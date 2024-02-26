#' Check Package Installation
#'
#' @description
#' - `check_pkg_installed()`: checks whether a package is installed and
#'   returns an error or `FALSE` if not available. If a package search is provided,
#'   the function will check whether a minimum version of a package is required.
#'
#' - `get_pkg_dependencies()` returns a tibble with all
#'   dependencies of a specific package.
#'
#' - `get_min_version_required()` will return, if any, the minimum version
#'   of `pkg` required by `reference_pkg`, `NULL` if no minimum version required.
#'
#' @param pkg (`string`)\cr
#'   name of required package
#' @param call (`environment`)\cr
#'   frame for error messaging. Default is [parent.frame()].
#' @param reference_pkg (`string`)\cr
#'   name of the package the function will search for a minimum required version from.
#' @param lib.loc (`path`)\cr
#'   location of `R` library trees to search through, see [utils::packageDescription()].
#'
#' @return `check_pkg_installed()` returns a logical or error, `get_min_version_required()`
#' returns `NULL` or a string with the minimum version required, `get_pkg_dependencies()`
#' returns a tibble.
#' @name check_pkg_installed
#'
#' @examples
#' check_pkg_installed("dplyr")
#'
#' is_pkg_installed("dplyr")
#'
#' get_pkg_dependencies()
#'
#' get_min_version_required("dplyr")
NULL

#' @rdname check_pkg_installed
#' @export
check_pkg_installed <- function(pkg,
                                reference_pkg = "cards",
                                call = parent.frame()) {
  # check if min version is required -------------------------------------------
  version <- get_min_version_required(pkg, reference_pkg)
  compare <- attr(version, "compare")

  # get fn name from which the function was called -----------------------------
  fn <- error_call(call)

  # prompt user to install package ---------------------------------------------
  rlang::check_installed(
    pkg = pkg,
    version = version,
    compare = compare,
    reason = switch(!is.null(fn),
      glue::glue("for `{fn}`")
    )
  )
  invisible()
}


#' @rdname check_pkg_installed
#' @export
is_pkg_installed <- function(pkg,
                             reference_pkg = "cards",
                             call = parent.frame()) {
  # check if min version is required -------------------------------------------
  version <- get_min_version_required(pkg, reference_pkg)
  compare <- attr(version, "compare")

  # get fn name from which the function was called -----------------------------
  fn <- tryCatch(
    paste0(as_label(evalq(sys.call(1L), envir = call)[[1]]), "()"),
    error = function(e) NULL
  )

  # check installation TRUE/FALSE ----------------------------------------------
  return(rlang::is_installed(pkg = pkg, version = version, compare = compare))
}

#' @rdname check_pkg_installed
#' @export
get_pkg_dependencies <- function(reference_pkg = "cards", lib.loc = NULL) {
  if (is.null(reference_pkg)) {
    return(NULL)
  }
  description <- utils::packageDescription(reference_pkg, lib.loc = lib.loc) |> suppressWarnings()
  if (identical(description, NA)) {
    return(NULL)
  }
  description |>
    unclass() |>
    dplyr::as_tibble() |>
    dplyr::select(
      any_of(c(
        "Package", "Version", "Imports", "Depends",
        "Suggests", "Enhances", "LinkingTo"
      ))
    ) |>
    dplyr::rename(
      reference_pkg = "Package",
      reference_pkg_version = "Version"
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("reference_pkg", "reference_pkg_version")),
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
get_min_version_required <- function(pkg, reference_pkg = "cards", lib.loc = NULL) {
  if (is.null(reference_pkg)) {
    return(NULL)
  }
  res <- get_pkg_dependencies(reference_pkg, lib.loc = lib.loc) |>
    dplyr::filter(.data$pkg == .env$pkg & !is.na(.data$version))
  if (nrow(res) == 0) {
    return(NULL)
  }
  version <- res[["version"]]
  attr(version, "compare") <- res[["compare"]]
  names(version) <- res[["dependency_type"]]
  version
}
