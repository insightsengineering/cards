

categorical_variable_summary_fns <- function(statistics = c("n", "p", "N")) {
  statistics <- rlang::arg_match(statistics, multiple = TRUE)

  tabulate_fn <-
    function(x, stats = statistics) {

      res <- list()
      if (any(c("N", "p") %in% stats))
        res[["N"]] <- length(x)
      if (any(c("n", "p") %in% stats))
        res[["n"]] <- table(x)
      if ("p" %in% stats)
        res$p <- res$n / res$N

      res
    }

  list(tabulate = tabulate_fn)
}

variable_level_summary_fns <- function(statistics = c("N_obs", "N_miss" , "N_nonmiss", "p_miss", "p_nonmiss")) {
  statistics <- rlang::arg_match(statistics, multiple = TRUE)

  list(
    var_level =
      function(x, stats = statistics) {
        res <- list()

        if (any(c("N_obs", "N_nonmiss", "p_miss", "p_nonmiss") %in% stats))
          res[["N_obs"]] <- length(x)
        if (any(c("N_miss", "N_nonmiss", "p_miss") %in% stats))
          res[["N_miss"]] <- sum(is.na(x))
        if (any(c("N_nonmiss", "p_nonmiss") %in% stats))
          res[["N_nonmiss"]] <- res[["N_obs"]] - res[["N_miss"]]
        if ("p_miss" %in% stats)
          res[["p_miss"]] <- res[["N_miss"]] / res[["N_obs"]]
        if ("p_nonmiss" %in% stats)
          res[["p_nonmiss"]] <- res[["N_nonmiss"]] / res[["N_obs"]]

        res
      }
  )
}
