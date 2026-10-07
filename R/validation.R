.check_number <- function(x, name, lower = 0, upper = Inf, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < lower || x > upper || (integer && x != floor(x))) {
    stop(name, " must be a finite ", if (integer) "integer" else "number",
         " between ", lower, " and ", upper, ".", call. = FALSE)
  }
}

.validate_data <- function(X, y, family, weights = NULL) {
  if (!is.data.frame(X) && !is.matrix(X)) stop("X must be a data frame or matrix.")
  X <- as.data.frame(X)
  if (nrow(X) < 2L || ncol(X) < 1L) stop("X needs at least two rows and one predictor.")
  if (anyNA(names(X)) || any(!nzchar(names(X))) || anyDuplicated(names(X)))
    stop("Predictor names must be nonempty and unique.")
  if (!all(vapply(X, function(z) is.numeric(z) && is.null(dim(z)), logical(1))))
    stop("Predictors must be numeric. Encode categorical variables explicitly.")
  if (any(!is.finite(as.matrix(X))))
    stop("X must contain only finite values. Handle missing values jointly before selection.")
  if (!is.numeric(y) || !is.null(dim(y)) || length(y) != nrow(X) || any(!is.finite(y)))
    stop("y must be a finite numeric vector with one value per row of X.")
  if (family == "binomial" && (!all(y %in% c(0, 1)) || length(unique(y)) != 2L))
    stop("Binomial y must contain both classes, coded 0 and 1.")
  if (!is.null(weights) && (!is.numeric(weights) || length(weights) != nrow(X) ||
      any(!is.finite(weights)) || any(weights < 0) || sum(weights > 0) < 2L))
    stop("weights must be finite, nonnegative, and positive for at least two rows.")
  X
}

.validate_search <- function(K, eps, delta, L) {
  .check_number(K, "K", integer = TRUE)
  .check_number(eps, "eps")
  .check_number(delta, "delta")
  if (!is.null(L)) .check_number(L, "L", lower = 1, integer = TRUE)
}

# Internal predictor names prevent collisions with the response and formula syntax.
.fit_selected <- function(X, y, vars, family, weights = NULL) {
  dat <- X[, vars, drop = FALSE]
  names(dat) <- if (length(vars)) paste0(".x", seq_along(vars)) else character(0)
  dat$.response <- y
  form <- if (length(vars)) .response ~ . else .response ~ 1
  if (family == "gaussian") {
    stats::lm(form, data = dat, weights = weights, na.action = stats::na.fail)
  } else {
    stats::glm(form, data = dat, family = stats::binomial(),
               weights = weights, na.action = stats::na.fail)
  }
}

.safe_ratio <- function(a, b) if (b == 0) NA_real_ else a / b

# Numeric-only inputs let search fits reuse one model matrix, avoiding repeated
# formula parsing and model.frame construction. Keep R's own AIC calculation.
.fit_candidate <- function(design, y, family, weights) {
  if (family == "gaussian") {
    fit <- if (is.null(weights)) stats::lm.fit(design, y) else
      stats::lm.wfit(design, y, w = weights)
    class(fit) <- "lm"
  } else {
    fit <- stats::glm.fit(design, y, family = stats::binomial(), weights = weights)
    class(fit) <- c("glm", "lm")
  }
  fit
}
