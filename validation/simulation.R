# Small reproducible sanity study, not a validation of error-rate guarantees.
library(multipathaic)
results <- list()
for (scenario in c("gaussian_signal", "gaussian_null", "gaussian_correlated", "binomial_signal")) {
  for (seed in 1:5) {
    set.seed(100 + seed)
    n <- 120; p <- 6
    X <- as.data.frame(matrix(rnorm(n * p), n, p)); names(X) <- paste0("x", seq_len(p))
    if (scenario == "gaussian_correlated") X$x2 <- 0.95 * X$x1 + sqrt(1 - 0.95^2) * X$x2
    family <- if (scenario == "binomial_signal") "binomial" else "gaussian"
    eta <- if (scenario == "gaussian_null") rep(0, n) else 1.5 * X$x1 - X$x3
    y <- if (family == "binomial") rbinom(n, 1, plogis(eta)) else eta + rnorm(n)
    train <- sample.int(n, 84)
    fit <- multipath_aic(X[train, ], y[train], family = family, B = 20, K = 6, L = 30, verbose = FALSE)
    m <- fit$plaus$plausible_models
    results[[length(results) + 1L]] <- data.frame(scenario = scenario, seed = seed,
      successes = fit$stab$B, models = nrow(m), best_size = if(nrow(m)) m$size[1] else NA,
      signal1_stability = fit$stab$pi[["x1"]], signal3_stability = fit$stab$pi[["x3"]],
      mean_noise_stability = mean(fit$stab$pi[c("x2", "x4", "x5", "x6")]))
  }
}
result <- do.call(rbind, results)
write.csv(result, "validation/simulation-results.csv", row.names = FALSE)
print(aggregate(result[, -c(1, 2)], list(scenario = result$scenario), mean, na.rm = TRUE))
