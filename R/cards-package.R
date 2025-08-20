#' @keywords internal
#' @import rlang
#' @importFrom dplyr across
#' @importFrom lifecycle deprecated
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c(".", "!<-", "parse_expr<-"))

release_bullets <- function() {
  c("Install package and re-build `pkgdown/index.Rmd`")
}
