# <U+2500><U+2500> 1. Load PR version and capture functions <U+2500><U+2500>
message("--- Loading PR version ---")
pkgload::load_all(".")
pr_version <- as.character(packageVersion("cards"))
message("PR version: ", pr_version)

pr_process_selectors <- cards::process_selectors.data.frame
pr_compute_formula_selector <- cards::compute_formula_selector
pr_data <- cards::ADSL

# Force-run to verify they work before unloading
pr_process_selectors(pr_data, variables = AGE, by = ARM)
pr_compute_formula_selector(pr_data, x = list(AGE ~ mean, ARM ~ unique), arg_name = "stat")
pkgload::unload("cards")

# <U+2500><U+2500> 2. Install and load main version, capture functions <U+2500><U+2500>
message("--- Installing main version ---")
remotes::install_github("insightsengineering/cards", quiet = TRUE)
library(cards)
main_version <- as.character(packageVersion("cards"))
message("Main version: ", main_version)

main_process_selectors <- cards::process_selectors.data.frame
main_compute_formula_selector <- cards::compute_formula_selector
main_data <- cards::ADSL

# <U+2500><U+2500> 3. Multi-round benchmarks <U+2500><U+2500>
set.seed(42)
n_rounds <- 5L

message("--- Running process_selectors benchmarks (", n_rounds, " rounds) ---")
selector_rounds <- lapply(seq_len(n_rounds), function(r) {
  message("  Round ", r)
  res <- bench::mark(
    `process_selectors bare symbols (main)` =
      main_process_selectors(main_data, variables = AGE, by = ARM),
    `process_selectors bare symbols (pr)` =
      pr_process_selectors(pr_data, variables = AGE, by = ARM),
    `process_selectors tidyselect (main)` =
      main_process_selectors(main_data, variables = starts_with("A"), by = dplyr::all_of("ARM")),
    `process_selectors tidyselect (pr)` =
      pr_process_selectors(pr_data, variables = starts_with("A"), by = dplyr::all_of("ARM")),
    iterations = 100,
    check = FALSE
  )
  data.frame(
    expression = as.character(res$expression),
    median_s   = as.numeric(res$median),
    round      = r
  )
}) |>
  do.call(what = rbind)

message("--- Running compute_formula_selector benchmarks (", n_rounds, " rounds) ---")
formula_rounds <- lapply(seq_len(n_rounds), function(r) {
  message("  Round ", r)
  res <- bench::mark(
    `formula_selector formulas (main)` =
      main_compute_formula_selector(main_data, x = list(AGE ~ mean, ARM ~ unique, SEX ~ unique), arg_name = "stat"),
    `formula_selector formulas (pr)` =
      pr_compute_formula_selector(pr_data, x = list(AGE ~ mean, ARM ~ unique, SEX ~ unique), arg_name = "stat"),
    `formula_selector named list (main)` =
      main_compute_formula_selector(main_data, x = list(AGE = mean, ARM = unique), arg_name = "stat"),
    `formula_selector named list (pr)` =
      pr_compute_formula_selector(pr_data, x = list(AGE = mean, ARM = unique), arg_name = "stat"),
    iterations = 100,
    check = FALSE
  )
  data.frame(
    expression = as.character(res$expression),
    median_s   = as.numeric(res$median),
    round      = r
  )
}) |>
  do.call(what = rbind)

# <U+2500><U+2500> 4. Build comparison table with confidence intervals <U+2500><U+2500>
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

selector_tab <- build_comparison(selector_rounds)
formula_tab <- build_comparison(formula_rounds)

header <- paste0(
  "## Performance Benchmark\n\n",
  "Comparing **main** (`", main_version, "`) vs **PR** (`", pr_version, "`)\n\n",
  "Each benchmark runs ", n_rounds, " independent rounds. ",
  "The **change** column shows the mean % difference (negative = faster).\n",
  "The **95% CI** column shows the confidence interval on the change. ",
  "If the CI excludes 0%, the result is flagged as a real improvement (\U2705) or regression (\U274C).\n\n"
)

selector_section <- paste0(
  "### process_selectors.data.frame()\n\n",
  paste(knitr::kable(selector_tab, format = "markdown"), collapse = "\n"),
  "\n\n"
)

formula_section <- paste0(
  "### compute_formula_selector()\n\n",
  paste(knitr::kable(formula_tab, format = "markdown"), collapse = "\n"),
  "\n"
)

report <- paste0(header, selector_section, formula_section)
writeLines(report, "bench_report.md")
cat(report)
