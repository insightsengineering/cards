# In the style of rlang's standalone-purrr.R, this file provides a minimal shim
# to provide a forcats-like API on top of base R functions.

fct_inorder <- function(f, ordered = NA) {
  factor(
    f,
    levels = stats::na.omit(unique(f)) |> union(levels(f)),
    ordered = ifelse(is.na(ordered), is.ordered(f), ordered)
  )
}
