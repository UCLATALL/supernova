# Benchmark: supernova() vs summary(lm()) in bootstrap-like loop
#
# This script measures the performance ratio between supernova() and
# the equivalent manual approach using summary(lm()). The target is
# that supernova() should be no more than 2x slower.
#
# Usage: Rscript benchmarks/supernova-benchmark.R

devtools::load_all()

cat("=== supernova() Performance Benchmark ===\n\n")

# --- Setup ---
set.seed(42)
n <- 200
data <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)

# --- Warmup: JIT compile all code paths ---
cat("Warming up...\n")
for (i in 1:50) {
  invisible(supernova(lm(y ~ x1, data = data)))
  invisible(supernova(lm(y ~ x1 + x2 + x3, data = data)))
  invisible(supernova(lm(y ~ x1 * x2, data = data)))
  invisible(summary(lm(y ~ x1, data = data)))
}
cat("Done.\n\n")

# --- Benchmark 1: Simple model (y ~ x1) ---
cat("Benchmark 1: Simple model (y ~ x1)\n")
fit_simple <- lm(y ~ x1, data = data)

n_iter <- 5000

t_summary <- system.time({
  for (i in seq_len(n_iter)) {
    s <- summary(fit_simple)
    s$fstatistic[[1]]
  }
})

t_supernova <- system.time({
  for (i in seq_len(n_iter)) {
    s <- supernova(fit_simple)
    s$tbl$F[1]
  }
})

ratio_simple <- t_supernova[["elapsed"]] / t_summary[["elapsed"]]
cat(sprintf("  summary():   %.3f sec\n", t_summary[["elapsed"]]))
cat(sprintf("  supernova(): %.3f sec\n", t_supernova[["elapsed"]]))
cat(sprintf("  Ratio:       %.2fx\n\n", ratio_simple))

# --- Benchmark 2: Multiple regression (y ~ x1 + x2 + x3) ---
cat("Benchmark 2: Multiple regression (y ~ x1 + x2 + x3)\n")
fit_multi <- lm(y ~ x1 + x2 + x3, data = data)

t_summary <- system.time({
  for (i in seq_len(n_iter)) {
    s <- summary(fit_multi)
    s$fstatistic[[1]]
  }
})

t_supernova <- system.time({
  for (i in seq_len(n_iter)) {
    s <- supernova(fit_multi)
    s$tbl$F[1]
  }
})

ratio_multi <- t_supernova[["elapsed"]] / t_summary[["elapsed"]]
cat(sprintf("  summary():   %.3f sec\n", t_summary[["elapsed"]]))
cat(sprintf("  supernova(): %.3f sec\n", t_supernova[["elapsed"]]))
cat(sprintf("  Ratio:       %.2fx\n\n", ratio_multi))

# --- Benchmark 3: Interactive model (y ~ x1 * x2) ---
cat("Benchmark 3: Interactive model (y ~ x1 * x2)\n")
fit_interact <- lm(y ~ x1 * x2, data = data)

t_summary <- system.time({
  for (i in seq_len(n_iter)) {
    s <- summary(fit_interact)
    s$fstatistic[[1]]
  }
})

t_supernova <- system.time({
  for (i in seq_len(n_iter)) {
    s <- supernova(fit_interact)
    s$tbl$F[1]
  }
})

ratio_interact <- t_supernova[["elapsed"]] / t_summary[["elapsed"]]
cat(sprintf("  summary():   %.3f sec\n", t_summary[["elapsed"]]))
cat(sprintf("  supernova(): %.3f sec\n", t_supernova[["elapsed"]]))
cat(sprintf("  Ratio:       %.2fx\n\n", ratio_interact))

# --- Summary ---
cat("=== Summary ===\n")
cat(sprintf("  Simple model ratio:      %.2fx (target: <= 2.0x)\n", ratio_simple))
cat(sprintf("  Multiple model ratio:    %.2fx (target: <= 2.0x)\n", ratio_multi))
cat(sprintf("  Interactive model ratio:  %.2fx (target: <= 2.0x)\n", ratio_interact))

all_pass <- all(c(ratio_simple, ratio_multi, ratio_interact) <= 2.0)
cat(sprintf("\n  Overall: %s\n", if (all_pass) "PASS" else "FAIL"))
