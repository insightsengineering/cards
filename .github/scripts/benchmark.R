# -- 1. Load PR version --
message("--- Loading PR version ---")
pkgload::load_all(".")
pr_version <- as.character(packageVersion("cards"))
message("PR version: ", pr_version)

pr_ard_summary <- cards::ard_summary
pr_data <- cards::ADSL

pr_ard_summary(data = pr_data, variables = AGE)
pkgload::unload("cards")

# -- 2. Install and load main version --
message("--- Installing main version ---")
remotes::install_github("insightsengineering/cards", quiet = TRUE)
library(cards)
main_version <- as.character(packageVersion("cards"))
message("Main version: ", main_version)

main_ard_summary <- cards::ard_summary
main_data <- cards::ADSL

# -- 3. Multi-round benchmarks --
set.seed(42)
n_rounds <- 5L

message("--- Running ard_summary benchmarks (", n_rounds, " rounds) ---")
bench_rounds <- lapply(seq_len(n_rounds), function(r) {
  message("  Round ", r)
  res <- bench::mark(
    `ard_summary (main)` = main_ard_summary(data = main_data, variables = AGE),
    `ard_summary (pr)` = pr_ard_summary(data = pr_data, variables = AGE),
    iterations = 100,
    check = FALSE
  )
  data.frame(
    expression = as.character(res$expression),
    median_s = as.numeric(res$median),
    round = r
  )
}) |>
  do.call(what = rbind)

# -- 4. Build comparison table with confidence intervals --
build_comparison <- function(rounds_df) {
  rounds_df$group <- sub(" \\((main|pr)\\)$", "", rounds_df$expression)
  rounds_df$version <- ifelse(
    grepl("(main)", rounds_df$expression, fixed = TRUE), "main", "pr"
  )

  groups <- unique(rounds_df$group)
  rows <- lapply(groups, function(g) {
    main_medians <- rounds_df$median_s[rounds_df$group == g & rounds_df$version == "main"]
    pr_medians <- rounds_df$median_s[rounds_df$group == g & rounds_df$version == "pr"]

    ratios <- pr_medians / main_medians
    mean_ratio <- mean(ratios)
    diff_pct <- (mean_ratio - 1) * 100

    n <- length(ratios)
    se <- sd(ratios) / sqrt(n)
    t_crit <- qt(0.975, df = n - 1)
    ci_lo <- (mean_ratio - t_crit * se - 1) * 100
    ci_hi <- (mean_ratio + t_crit * se - 1) * 100

    if (ci_hi < 0) {
      verdict <- paste0("\U2705 ", round(diff_pct, 1), "%")
    } else if (ci_lo > 0) {
      verdict <- paste0("\U274C +", round(diff_pct, 1), "%")
    } else {
      sign_chr <- ifelse(diff_pct >= 0, "+", "")
      verdict <- paste0("\U2796 ", sign_chr, round(diff_pct, 1), "%")
    }

    data.frame(
      expression = g,
      main = paste0(round(mean(main_medians) * 1000, 2), "ms"),
      pr = paste0(round(mean(pr_medians) * 1000, 2), "ms"),
      change = verdict,
      ci = paste0("[", round(ci_lo, 1), "%, ", round(ci_hi, 1), "%]")
    )
  })
  do.call(rbind, rows)
}

bench_tab <- build_comparison(bench_rounds)

header <- paste0(
  "## Performance Benchmark\n\n",
  "Comparing **main** (`", main_version, "`) vs **PR** (`", pr_version, "`)\n\n",
  "Each benchmark runs ", n_rounds, " independent rounds. ",
  "The **change** column shows the mean % difference (negative = faster).\n",
  "The **95% CI** column shows the confidence interval on the change. ",
  "If the CI excludes 0%, the result is flagged as a real improvement (\U2705) or regression (\U274C).\n\n"
)

bench_section <- paste0(
  "### ard_summary()\n\n",
  paste(knitr::kable(bench_tab, format = "markdown"), collapse = "\n"),
  "\n"
)

report <- paste0(header, bench_section)
writeLines(report, "bench_report.md")
cat(report)
