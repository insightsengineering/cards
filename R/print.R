#' Print Methods
#'
#' @param x a cards table
#' @param ... Not used
#'
#' @return a gt table
#' @export
#'
#' @examples
#' # TODO: add example
print.cards <- function(x, ...) {
  convert_cards(x)
}
