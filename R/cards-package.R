#' @keywords internal
#' @import rlang
#' @importFrom dplyr across
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c(".", "!<-", "parse_expr<-"))

release_bullets <- function() {
  c("Build `pkgdown/index.Rmd`")
}
