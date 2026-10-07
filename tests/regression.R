library(multipathaic)
expect_error <- function(expr, pattern) {
  message <- tryCatch({ force(expr); NA_character_ }, error = conditionMessage)
  stopifnot(!is.na(message), grepl(pattern, message, fixed = TRUE))
}
set.seed(13)
X <- data.frame(signal = rnorm(90), noise = rnorm(90), other = rnorm(90))
y <- 1.1 * X$signal + rnorm(90)
# Every retained one-step candidate must improve its own parent, even at large delta.
f <- build_paths(X, y, K = 3, delta = 1e5, verbose = FALSE)
for (i in seq_len(nrow(f$all_models))) {
  m <- f$all_models$model[[i]]
  dat <- data.frame(y = y, X[, m, drop = FALSE])
  ref <- lm(if (length(m)) y ~ . else y ~ 1, dat)
  stopifnot(isTRUE(all.equal(f$all_models$AIC[i], AIC(ref), tolerance = 1e-10)))
  if (length(m)) {
    parents <- f$all_models[f$all_models$size == length(m) - 1L, ]
    valid <- vapply(parents$model, function(p) all(p %in% m), logical(1))
    stopifnot(any(parents$AIC[valid] - f$all_models$AIC[i] >= 1e-6))
  }
}
# Null models and empty plausible sets must have consistent table shapes.
null <- build_paths(X, y, K = 0, verbose = FALSE)
stopifnot(nrow(null$all_models) == 1L, identical(names(null$aic_by_model), ""))
p <- plausible_models(null, verbose = FALSE)
stopifnot(nrow(p$plausible_models) == 1L, nrow(p$summary) == 0L)
p <- plausible_models(null, pi = c(signal = 0, noise = 0, other = 0), verbose = FALSE)
stopifnot(nrow(p$plausible_models) == 1L, is.na(p$plausible_models$avg_stability))
p <- plausible_models(f, pi = c(signal = 0, noise = 0, other = 0), Delta = 0, verbose = FALSE)
stopifnot(nrow(p$plausible_models) == 0L, nrow(p$summary) == 0L)
# Names containing formula syntax, response name, and the old key separator are safe.
weird <- X
names(weird) <- c("y", "a|b", "a%7Cb")
fw <- build_paths(weird, y, delta = 1e5, verbose = FALSE)
stopifnot(isTRUE(all.equal(sort(f$all_models$AIC), sort(fw$all_models$AIC))))
set.seed(9)
st <- stability(as.matrix(unname(X)), y, B = 4, verbose = FALSE)
stopifnot(length(st$pi) == 3L, !anyNA(st$pi), all(st$pi >= 0 & st$pi <= 1))
# Weighted AIC agrees with R on the same rows.
w <- seq(0.5, 1.5, length.out = nrow(X))
fw <- build_paths(X, y, weights = w, K = 1, delta = 1000, verbose = FALSE)
stopifnot(abs(fw$all_models$AIC[1] - AIC(lm(y ~ 1, weights = w))) < 1e-8)
# Validate data before any model-specific row omission or resampling.
missing <- X; missing$noise[1] <- NA
expect_error(build_paths(missing, y, verbose = FALSE), "finite")
expect_error(build_paths(X, y[-1], verbose = FALSE), "one value per row")
expect_error(build_paths(transform(X, noise = letters[1:90]), y), "numeric")
expect_error(build_paths(X, y, L = 0), "L")
expect_error(build_paths(X, y, K = -1), "K")
expect_error(stability(X, y, B = 0), "B")
expect_error(stability(X, y, resample_fraction = 0.001), "at least two")
expect_error(plausible_models(f, pi = c(noise = 1)), "cover every")
# Reproducibility, and zero stability for valid intercept-only bootstrap runs.
set.seed(42); a <- stability(X, y, B = 5, verbose = FALSE)
set.seed(42); b <- stability(X, y, B = 5, verbose = FALSE)
stopifnot(identical(a$pi, b$pi), a$B == 5, a$B_requested == 5)
a <- stability(X, y, B = 3, K = 0, verbose = FALSE)
stopifnot(all(a$pi == 0), a$B == 3)
# A rare binary class produces failed single-class draws; denominator counts successes.
rare_y <- c(1, rep(0, nrow(X) - 1L))
set.seed(4)
warnings <- character()
a <- withCallingHandlers(stability(X, rare_y, family = "binomial", B = 30, K = 0, verbose = FALSE),
  warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
stopifnot(a$B > 0, a$B < 30, length(a$failures) == 30 - a$B,
          any(grepl("resamples failed", warnings)), identical(a$pi_sum / a$B, a$pi))
# Logistic AIC and confusion metrics agree with an independent direct fit.
set.seed(41)
yb <- rbinom(nrow(X), 1, plogis(0.8 * X$signal))
result <- multipath_aic(weird, yb, family = "binomial", B = 5, tau = 0, verbose = FALSE)
vars <- result$plaus$plausible_models$model[[1]]
dat <- weird[, vars, drop = FALSE]; names(dat) <- paste0("x", seq_along(vars)); dat$response <- yb
ref <- glm(response ~ ., dat, family = binomial())
pred <- predict(ref, type = "response") >= 0.5
metrics <- confusion_metrics(result, verbose = FALSE)
stopifnot(metrics$Accuracy == round(mean(pred == yb), 3))
expect_error(confusion_metrics(result, model_index = 0), "model_index")
expect_error(confusion_metrics(result, cutoff = 2), "cutoff")
expect_error(confusion_metrics(list(forest = f)), "binomial")
result <- multipath_aic(X, yb, family = "binomial", K = 0, B = 3, verbose = FALSE)
metrics <- confusion_metrics(result, cutoff = 1, verbose = FALSE)
stopifnot(is.na(metrics$FDR), is.na(metrics$DOR), metrics$Sensitivity == 0)
cat("Core regression checks passed.\n")
# Controlled failures isolate denominator accounting from stochastic fitting failures.
ns <- asNamespace("multipathaic")
env <- new.env(parent = ns)
mock_stability <- get("stability", ns)
environment(mock_stability) <- env
env$iteration <- 0L
env$build_paths <- function(...) {
  env$iteration <- env$iteration + 1L
  if (env$iteration == 2L) stop("controlled failure")
  models <- if (env$iteration == 1L) list("signal") else list("signal", "noise")
  list(path_forest = list(frontiers = list(data.frame(model = I(models)))))
}
a <- suppressWarnings(mock_stability(X, y, B = 3, verbose = FALSE))
stopifnot(a$B == 2, a$B_requested == 3, a$pi[["signal"]] == 0.75,
          a$pi[["noise"]] == 0.25, length(a$failures) == 1)
env$build_paths <- function(...) stop("controlled failure")
expect_error(mock_stability(X, y, B = 2, verbose = FALSE), "All bootstrap resamples failed")
cat("Failure-accounting checks passed.\n")
# Numeric fitting optimization must preserve R's AIC for all retained GLMs.
for (weighted in c(FALSE, TRUE)) {
  w <- if (weighted) rep(c(0, 1, 2), length.out = nrow(X)) else NULL
  f <- build_paths(X, yb, family = "binomial", weights = w, delta = 20, verbose = FALSE)
  for (i in seq_len(nrow(f$all_models))) {
    vars <- f$all_models$model[[i]]
    d <- data.frame(y = yb, X[, vars, drop = FALSE])
    ref <- glm(if (length(vars)) y ~ . else y ~ 1, data = d, family = binomial(), weights = w)
    stopifnot(abs(AIC(ref) - f$all_models$AIC[i]) < 1e-8)
  }
}
w <- rep(c(0, 0.5, 2), length.out = nrow(X))
f <- build_paths(X, y, weights = w, delta = 20, verbose = FALSE)
for (i in seq_len(nrow(f$all_models))) {
  vars <- f$all_models$model[[i]]
  d <- data.frame(y = y, X[, vars, drop = FALSE])
  ref <- lm(if (length(vars)) y ~ . else y ~ 1, data = d, weights = w)
  stopifnot(abs(AIC(ref) - f$all_models$AIC[i]) < 1e-8)
}
cat("Optimized fitting agrees with direct lm/glm AIC.\n")
