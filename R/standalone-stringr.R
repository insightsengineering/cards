# In the style of rlang's standalone-purrr.R, this file provides a minimal shim
# to provide a stringr-like API on top of base R functions.

str_trim <- function(string, side = c("both", "left", "right")) {
  side <- rlang::arg_match(side)
  trimws(x = string, which = side, whitespace = "[ \t\r\n]")
}

str_squish <- function(string) {
  gsub(x = string, pattern = "\\s+", replacement = " ") |>
    str_trim(side = "both")
}

str_remove_all <- function(string, pattern) {
  gsub(x = string, pattern = pattern, replacement = "")
}

str_extract <- function(string, pattern) {
  ifelse(
    str_detect(string, pattern),
    regmatches(x = string, m = regexpr(pattern = pattern, text = string)),
    NA_character_
  )
}

str_detect <- function(string, pattern) {
  grepl(pattern = pattern, x = string)
}




