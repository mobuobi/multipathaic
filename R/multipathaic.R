#' Multi-Path Forward Selection Using AIC
#'
#' @description
#' Implements a branching forward-selection algorithm using AIC for either
#' linear (`gaussian`) or logistic (`binomial`) regression models.
#'
#' At each step, every current parent model attempts to add every unused
#' predictor, producing a set of children models. Children are kept if they:
#' (1) improve AIC relative to the parent by at least `eps`, and
#' (2) lie within `delta` of the best child AIC for that parent.
#'
#' Duplicate models are merged, and if more than `L` children remain, only
#' the `L` best (lowest AIC) models are kept. The process continues for up
#' to `K` steps or until no parent improves AIC.
#'
#' @param X A data frame or matrix of predictors. All predictors must be numeric
#'   or convertible to numeric.
#' @param y A numeric response vector. For `family = "binomial"`, must be 0/1.
#' @param family Character string: either `"gaussian"` (linear regression) or
#'   `"binomial"` (logistic regression).
#' @param K Integer. Maximum number of forward-selection steps to explore.
#'   Default: \code{min(ncol(X), 10)}.
#' @param eps Minimum AIC improvement required for a child model to be
#'   considered an improvement over its parent. Default: `1e-6`.
#' @param delta AIC tolerance for keeping near-best children at each step.
#'   Children with AIC <= (best child AIC + delta) are retained. Default: 2.
#' @param L Maximum number of child models retained at each step after
#'   deduplication. Helps limit tree size. Default: 100.
#' @param weights Optional numeric vector of observation weights for the
#'   underlying `lm()` or `glm()` fits.
#' @param verbose Logical. If TRUE (default), prints detailed information
#'   about each selection step, including candidate AICs and parent/child
#'   summaries.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{path_forest}{List with `frontiers` component containing models at each step.}
#'   \item{aic_by_model}{Named list of AIC values indexed by model key.}
#'   \item{all_models}{A combined data frame of all models explored across
#'     all steps, sorted by size and AIC.}
#'   \item{meta}{A list of the parameters used (`K`, `eps`, `delta`,
#'     `L`, and `family`).}
#' }
#'
#' @details
#' This function constructs a *model forest* representing multiple competitive
#' forward-selection paths, rather than a single greedy stepwise sequence.
#'
#' For each parent model:
#' - All possible one-variable additions are evaluated.
#' - Children that pass AIC filters (`eps` and `delta`) are retained.
#' - Duplicates are merged by retaining the best AIC among identical
#'   variable sets.
#'
#' The algorithm stops early if no parent produces an improved child.
#'
#' @examples
#' ## Linear regression example
#' set.seed(1)
#' n <- 150; p <- 8
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' beta <- c(2, -1.5, 0, 0, 1, rep(0, p-5))
#' y <- as.numeric(as.matrix(X) %*% beta + rnorm(n, 1))
#'
#' forest <- build_paths(
#'   X, y,
#'   family = "gaussian",
#'   K = 10, eps = 1e-6, delta = 2, L = 100,
#'   verbose = TRUE
#' )
#'
#' ## Logistic regression example
#' set.seed(1)
#' n <- 200; p <- 8
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' eta <- 1.5*X$x1 - 2*X$x2 + 1*X$x5
#' prob <- 1/(1 + exp(-eta))
#' y <- rbinom(n, 1, prob)
#'
#' forest <- build_paths(
#'   X, y,
#'   family = "binomial",
#'   K = 10, eps = 1e-6, delta = 2, L = 100,
#'   verbose = TRUE
#' )
#'
#' @export
build_paths <- function(
    X, y,
    family = c("gaussian", "binomial"),
    K = min(ncol(X), 10),
    eps = 1e-6,
    delta = 2,
    L = 100,
    weights = NULL,
    verbose = TRUE
) {
  family <- match.arg(family)
  X <- as.data.frame(X)
  pnames <- colnames(X); if (is.null(pnames)) pnames <- paste0("V", seq_len(ncol(X)))
  colnames(X) <- pnames

  # internal helper: fit model and compute AIC
  model_aic <- function(vars) {
    df <- if (length(vars)) X[, vars, drop = FALSE] else X[, 0, drop = FALSE]
    dat <- data.frame(y = y, df)
    fit <- if (family == "gaussian")
      stats::lm(y ~ ., data = dat, weights = weights)
    else
      stats::glm(y ~ ., data = dat, family = stats::binomial(), weights = weights)
    as.numeric(stats::AIC(fit))
  }

  # represent model as unique key
  key_of <- function(vars) paste(sort(vars), collapse = "|")

  # Step 0: start with intercept-only model
  base_aic <- model_aic(character(0))
  current_parents <- data.frame(
    key = key_of(character(0)),
    AIC = base_aic,
    size = 0,
    model = I(list(character(0))),
    stringsAsFactors = FALSE
  )

  if (verbose) cat(sprintf("Step 0: Intercept-only AIC = %.4f\n", base_aic))

  frontiers <- list()
  all_models <- current_parents
  aic_by_model <- list()

  for (k in seq_len(K)) {
    if (verbose) {
      cat(sprintf("\n--- Step %d ---\n", k))
      cat(sprintf("Parents this step: %d\n", nrow(current_parents)))
    }

    children_list <- list()
    any_parent_improved <- FALSE

    # loop through each parent model
    for (i in seq_len(nrow(current_parents))) {
      parent_vars <- current_parents$model[[i]]
      parent_aic  <- current_parents$AIC[i]
      parent_key  <- current_parents$key[i]
      remaining   <- setdiff(pnames, parent_vars)
      if (!length(remaining)) next

      # evaluate all single-variable additions
      cand <- lapply(remaining, function(v) {
        new_vars <- c(parent_vars, v)
        a <- model_aic(new_vars)
        data.frame(
          parent_key = parent_key,
          parent_size = length(parent_vars),
          parent_aic  = parent_aic,
          added_var   = v,
          key         = key_of(new_vars),
          AIC         = a,
          size        = length(new_vars),
          stringsAsFactors = FALSE
        )
      })
      cand <- do.call(rbind, cand)

      # best child & diagnostics
      best_idx <- which.min(cand$AIC)
      best_aic <- cand$AIC[best_idx]
      cand$delta_from_best <- cand$AIC - best_aic
      parent_improved <- (parent_aic - best_aic) >= eps
      cand$kept <- parent_improved & (cand$AIC <= (best_aic + delta))

      # print per-parent summary
      if (verbose) {
        parent_label <- if (length(parent_vars)) paste(parent_vars, collapse = " + ") else "(intercept)"
        cat(sprintf("\nParent %d: %s | AIC = %.4f\n", i, parent_label, parent_aic))
        cat("  Candidates (sorted by child AIC):\n")
        to_show <- cand[order(cand$AIC), c("added_var", "AIC", "delta_from_best", "kept")]
        rownames(to_show) <- NULL
        print(to_show, row.names = FALSE)
        if (!parent_improved) cat("   No child improves AIC by ≥ eps; parent does not expand.\n")
      }

      # keep near-ties for next step
      if (parent_improved) {
        any_parent_improved <- TRUE
        keep <- cand[cand$kept, ]
        if (nrow(keep)) {
          children_list[[length(children_list) + 1]] <-
            data.frame(
              key = keep$key,
              AIC = keep$AIC,
              size = keep$size,
              model = I(lapply(keep$key, function(k) unlist(strsplit(k, "\\|"), use.names = FALSE))),
              stringsAsFactors = FALSE
            )
        }
      }
    }

    if (!any_parent_improved) {
      if (verbose) cat("\nNo parent improved by ≥ eps; stopping early.\n")
      break
    }

    # combine all children (raw before dedup)
    pre_children <- do.call(rbind, children_list)
    n_raw <- nrow(pre_children)
    best_aic_raw <- min(pre_children$AIC)
    worst_aic_raw <- max(pre_children$AIC)

    # remove duplicates and keep best AIC if duplicates exist
    children <- pre_children[order(pre_children$key, pre_children$AIC), ]
    children <- children[!duplicated(children$key), ]
    n_unique <- nrow(children)
    n_merged <- n_raw - n_unique

    # cap to top L
    if (!is.null(L) && n_unique > L) {
      children <- children[order(children$AIC), ][seq_len(L), ]
    }

    if (verbose) {
      cat(sprintf("\nGenerated %d children (unique %d, merged %d duplicate%s)\n",
                  n_raw, n_unique, n_merged, ifelse(n_merged == 1, "", "s")))
      cat(sprintf("   Best AIC before dedup = %.4f, Worst AIC before dedup = %.4f\n",
                  best_aic_raw, worst_aic_raw))
      cat(sprintf("   Best AIC after dedup  = %.4f, Worst AIC after dedup  = %.4f\n",
                  min(children$AIC), max(children$AIC)))
    }

    frontiers[[k]] <- children
    all_models <- rbind(all_models, children)
    current_parents <- children
  }

  # Final clean up
  all_models <- all_models[order(all_models$size, all_models$AIC), ]
  rownames(all_models) <- NULL

  # Create aic_by_model list
  for (i in seq_len(nrow(all_models))) {
    aic_by_model[[all_models$key[i]]] <- all_models$AIC[i]
  }

  list(
    path_forest = list(frontiers = frontiers),
    aic_by_model = aic_by_model,
    all_models = all_models,
    meta = list(K=K, eps=eps, delta=delta, L=L, family=family)
  )
}


#' Stability Estimation via Resampled Multi-Path Forward Selection
#'
#' @description
#' Computes variable-selection stability by repeatedly resampling the data
#' and applying the `build_paths()` algorithm.
#'
#' For each bootstrap or subsample replicate, the function runs the
#' multi-path forward-selection search, extracts the variables appearing in the
#' final step of the search path, and computes the proportion of models containing
#' each variable (z_j^(b)). The stability score is the average of these proportions
#' across all resamples.
#'
#' Stability is defined as:
#' \deqn{ \pi_j = \frac{1}{B} \sum_{b=1}^{B} z_j^{(b)} }
#' where z_j^(b) is the proportion of models in resample b that contain variable j.
#'
#' @param X A data frame or matrix of predictors.
#' @param y A numeric outcome vector for `gaussian`, or 0/1 for `binomial`.
#' @param family Model family, either `"gaussian"` (linear) or `"binomial"` (logistic).
#' @param K Maximum number of forward-selection steps in each resample.
#'   Default: \code{min(ncol(X), 10)}.
#' @param eps Minimum AIC improvement required for a parent model
#'   to expand. Default: `1e-6`.
#' @param delta AIC tolerance controlling which near-best models are kept. Default: 2.
#' @param L Maximum number of child models kept at each step. Default: 100.
#' @param B Number of resamples for stability estimation. Default: 100.
#' @param resample_fraction Fraction of the dataset used per resample
#'   (bootstrap or subsample). Default: `0.8`.
#' @param verbose Logical. If TRUE, prints progress for each resample. Default: TRUE.
#'
#' @details
#' Each resampled dataset is fit using the same multi-path forward-selection
#' procedure as in Algorithm 1. For each resample, the proportion of models
#' containing each variable is computed (z_j^(b)), and these proportions are
#' averaged across all B resamples to obtain the final stability scores (pi_j).
#'
#' When a resample fails (e.g., due to singularities), it is skipped with a warning.
#'
#' @return
#' A list with:
#' \describe{
#'   \item{pi}{Named vector of stability scores, sorted decreasing.}
#'   \item{pi_sum}{Running sum used to compute pi (for diagnostic purposes).}
#'   \item{B}{Number of completed resample iterations.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @examples
#' ## Linear regression example
#' set.seed(123)
#' n <- 150; p <- 10
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' beta <- c(2, -1.5, 0, 0, 1, rep(0, p-5))
#' y <- as.numeric(as.matrix(X) %*% beta + rnorm(n, 1))
#'
#' stab <- stability(
#'   X, y,
#'   family = "gaussian",
#'   K = 10,
#'   B = 50,
#'   verbose = TRUE
#' )
#'
#' ## Logistic regression example
#' set.seed(1)
#' n <- 200; p <- 10
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' eta <- 1.5*X$x1 - 2*X$x2 + 1*X$x5
#' prob <- 1/(1 + exp(-eta))
#' y <- rbinom(n, 1, prob)
#'
#' stab <- stability(
#'   X, y,
#'   family = "binomial",
#'   K = 8,
#'   B = 100,
#'   verbose = TRUE
#' )
#'
#' @export
stability <- function(X, y,
                      family = c("gaussian", "binomial"),
                      K = min(ncol(X), 10),
                      eps = 1e-6,
                      delta = 2,
                      L = 100,
                      B = 100,
                      resample_fraction = 0.8,
                      verbose = TRUE) {

  # Match family argument
  family <- match.arg(family)

  # --- Initialize ---
  p <- ncol(X)
  pnames <- colnames(X)
  if (is.null(pnames)) pnames <- paste0("V", seq_len(p))

  pi_sum <- setNames(numeric(p), pnames)  # running sum for pi_j

  if (verbose) {
    cat("Running stability estimation with", B, "resamples...\n\n")
  }

  # --- Main loop ---
  for (b in seq_len(B)) {
    if (verbose && b %% 10 == 0) cat("Resample", b, "of", B, "...\n")

    # Step 1: Resample the data (bootstrap)
    idx <- sample(1:nrow(X),
                  size = floor(resample_fraction * nrow(X)),
                  replace = TRUE)
    Xb <- X[idx, , drop = FALSE]
    yb <- y[idx]

    # Step 2: Run Multi-Path Forward Selection on resample
    forest_b <- tryCatch({
      build_paths(
        Xb, yb,
        family = family,
        K = K,
        eps = eps,
        delta = delta,
        L = L,
        verbose = FALSE
      )
    }, error = function(e) {
      if (verbose) cat("  Warning: iteration", b, "failed, skipping.\n")
      return(NULL)
    })

    # Step 3: Compute z_j^(b) = proportion of models containing variable j
    if (!is.null(forest_b) && length(forest_b$path_forest$frontiers) > 0) {
      last_frontier <- forest_b$path_forest$frontiers[[length(forest_b$path_forest$frontiers)]]

      if (nrow(last_frontier) > 0) {
        # Extract all variables from all models at final step
        all_vars <- unlist(strsplit(last_frontier$key, "\\|"))
        n_models <- nrow(last_frontier)

        # Count occurrences of each variable across models
        var_counts <- table(all_vars)

        # Compute z_j^(b) = proportion of models containing j
        z_b <- var_counts / n_models

        # Add to running sum
        pi_sum[names(z_b)] <- pi_sum[names(z_b)] + z_b
      }
    }
  }

  # --- Compute stability scores: pi_j = (1/B) * sum(z_j^(b)) ---
  pi <- pi_sum / B
  pi <- sort(pi, decreasing = TRUE)

  # --- Output ---
  if (verbose) {
    cat("\nCompleted", B, "resamples.\n")
    cat("Top stable variables:\n")
    print(round(pi, 3))
  }

  return(list(
    pi = pi,
    pi_sum = pi_sum,
    B = B,
    call = match.call()
  ))
}


#' Select Plausible and Stable Models from Multi-Path AIC Search
#'
#' @description
#' This function implements *Algorithm 3* of the multi-path AIC procedure.
#' It filters the full model set created by `build_paths()`
#' using two criteria:
#'
#' \enumerate{
#'   \item \strong{AIC Plausibility}:
#'         Keep all models whose AIC is within \eqn{\Delta} of the best AIC.
#'   \item \strong{Stability Threshold} (optional):
#'         If variable stability scores are supplied (from `stability()`),
#'         then models whose average stability is below \eqn{\tau} are removed.
#' }
#'
#' The function also computes variable inclusion probabilities across all
#' remaining plausible models.
#'
#' @param forest A result object from `build_paths()`, containing `all_models`.
#' @param pi Optional named vector of per-variable stability values
#'   (from `stability()$pi`). Default: `NULL`.
#' @param Delta AIC tolerance \eqn{\Delta} defining the plausible region.
#'   Models with AIC ≤ (best AIC + Delta) are kept. Default: `2`.
#' @param tau Minimum average model stability \eqn{\tau} required to retain
#'   a model (if stability scores supplied). Default: `0.6`.
#' @param verbose Logical. If TRUE, prints detailed progress and summaries. Default: TRUE.
#'
#' @details
#' After AIC filtering (Steps 1–3), the function optionally applies a stability filter
#' (Steps 4–5).
#' For each plausible model, the "average stability" is computed as:
#'
#' \deqn{ \text{avg\_stability}(M) = \frac{1}{|M|} \sum_{v \in M} \pi_v }
#'
#' If `pi = NULL`, the stability-based filtering is skipped.
#'
#' The function also calculates variable inclusion probabilities across all
#' final plausible models (Step 6). Variables appearing in all models are marked
#' with no indicator; those appearing in only a subset are marked with `"(-)"`.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{plausible_models}{Data frame of plausible (and optionally stable) models.}
#'   \item{inclusion}{Named vector of variable inclusion probabilities.}
#'   \item{summary}{Data frame combining stability and inclusion probabilities.}
#'   \item{Delta}{The AIC tolerance used.}
#'   \item{tau}{The stability threshold used.}
#'   \item{best_aic}{The best (minimum) AIC observed.}
#' }
#'
#' @examples
#' ## Complete pipeline example
#' set.seed(123)
#' n <- 120; p <- 6
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' beta <- c(2, -1, 0, 0, 1, 0)
#' y <- as.numeric(as.matrix(X) %*% beta + rnorm(n))
#'
#' # Algorithm 1: multi-path forward selection
#' forest <- build_paths(X, y, family = "gaussian", K = 6, verbose = FALSE)
#'
#' # Algorithm 2: variable stability
#' stab <- stability(X, y, family = "gaussian", B = 20, verbose = FALSE)
#'
#' # Algorithm 3: plausible models
#' plaus <- plausible_models(
#'   forest = forest,
#'   pi = stab$pi,
#'   Delta = 2,
#'   tau = 0.6,
#'   verbose = TRUE
#' )
#'
#' @export
plausible_models <- function(forest,
                             pi = NULL,
                             Delta = 2,
                             tau = 0.6,
                             verbose = TRUE) {
  # --- Sanity check ---
  if (is.null(forest$all_models) || !"AIC" %in% colnames(forest$all_models)) {
    stop("forest must be a result object from build_paths()")
  }

  all_models <- forest$all_models

  # --- Step 1–3: AIC-based plausibility ---
  best_aic <- min(all_models$AIC, na.rm = TRUE)
  cutoff <- best_aic + Delta

  plausible <- subset(all_models, AIC <= cutoff)
  plausible <- plausible[order(plausible$AIC), ]

  if (verbose) {
    cat("Best AIC =", round(best_aic, 3), "\n")
    cat("Cutoff (Delta =", Delta, ") =", round(cutoff, 3), "\n")
    cat("Number of plausible models before stability filter:", nrow(plausible), "\n\n")
  }

  # --- Step 4: Compute model-level average stability (if available) ---
  if (!is.null(pi)) {
    plausible$avg_stability <- sapply(plausible$model, function(m) {
      if (length(m) == 0) return(0)
      mean(pi[m], na.rm = TRUE)
    })

    # --- Step 5: Filter models by tau stability threshold ---
    before_filter <- nrow(plausible)
    plausible <- subset(plausible, avg_stability >= tau)
    after_filter <- nrow(plausible)

    if (verbose) {
      cat(sprintf("Applied stability threshold tau = %.2f\n", tau))
      cat(sprintf("Models kept after stability filter: %d (removed %d)\n\n",
                  after_filter, before_filter - after_filter))
    }
  } else {
    plausible$avg_stability <- NA
  }

  # --- Step 6: Variable inclusion probabilities ---
  if (nrow(plausible) > 0) {
    vars <- unique(unlist(plausible$model))
    inclusion <- setNames(numeric(length(vars)), vars)

    for (v in vars) {
      inclusion[v] <- mean(sapply(plausible$model, function(m) v %in% m))
    }
    inclusion <- sort(inclusion, decreasing = TRUE)
  } else {
    inclusion <- numeric(0)
  }

  # --- Step 7: Summary table ---
  marked_vars <- ifelse(inclusion < 1, paste0(names(inclusion), " (-)"), names(inclusion))

  summary_table <- data.frame(
    Variable = marked_vars,
    InclusionProb = round(inclusion, 3),
    Stability = if (!is.null(pi[names(inclusion)]))
      round(pi[names(inclusion)], 3)
    else NA,
    stringsAsFactors = FALSE
  )

  # --- Verbose summary ---
  if (verbose) {
    cat("Final plausible stable models:", nrow(plausible), "\n")
    if (nrow(plausible) > 0) {
      cat("Average model stabilities (first few):\n")
      print(head(plausible[, c("AIC", "avg_stability")], 10))
    }
    cat("\nVariable inclusion probabilities:\n")
    print(summary_table)

    if (nrow(plausible) > 0) {
      cat("\nAll plausible models (Delta ≤", Delta, "):\n")
      for (i in seq_len(nrow(plausible))) {
        model_id <- rownames(plausible)[i]
        cat(sprintf("Model ID %s: AIC = %.3f | Variables = %s\n",
                    model_id, plausible$AIC[i],
                    paste(plausible$model[[i]], collapse = ", ")))
      }
    }
  }

  # --- Output ---
  return(list(
    plausible_models = plausible,
    inclusion = inclusion,
    summary = summary_table,
    Delta = Delta,
    tau = tau,
    best_aic = best_aic
  ))
}


#' Overall Multi-Path AIC Selection Procedure
#'
#' @description
#' This function implements the Multi-Path AIC Search Framework.
#' It combines the three main components:
#'
#' \enumerate{
#'   \item \strong{Multi-Path Forward Selection} (`build_paths()`)
#'   \item \strong{Stability Estimation via Resampling} (`stability()`)
#'   \item \strong{Plausible Model Selection} (`plausible_models()`)
#' }
#'
#' The output contains all intermediate results and the final set of
#' plausible, stable models, along with the full search record and variable
#' stability summaries.
#'
#' @param X Predictor matrix or data frame.
#' @param y Response variable (numeric for Gaussian, binary 0/1 for Binomial).
#' @param family Model family: `"gaussian"` or `"binomial"`.
#' @param K Maximum number of forward selection steps.
#'   Default: \code{min(ncol(X), 10)}.
#' @param eps Minimum AIC improvement required for a model expansion.
#' @param delta AIC "near-tie" threshold for branching.
#' @param L Maximum number of child models retained at each step.
#' @param B Number of resamples in stability estimation.
#' @param resample_fraction Fraction of rows sampled at each stability iteration.
#' @param Delta AIC tolerance for selecting plausible models.
#' @param tau Stability threshold for filtering plausible models.
#' @param verbose Logical; if TRUE, prints progress summaries.
#'
#' @details
#' The procedure works as follows:
#'
#' \strong{Step 1: Multi-Path Forward Selection}
#' Generates a branching set of candidate models using AIC-based exploration.
#'
#' \strong{Step 2: Stability Estimation}
#' Evaluates how frequently each variable is selected across \(B\) resamples.
#'
#' \strong{Step 3: Plausible Model Selection}
#' Combines AIC plausibility and stability screening to identify a final set
#' of stable and competitive models.
#'
#' \strong{Step 4: Compile and Return Results}
#' Outputs a structured object containing the full path search,
#' stability scores, and the final plausible model set.
#'
#' @return
#' A list with the following components:
#' \describe{
#'   \item{forest}{Output of \code{build_paths()}.}
#'   \item{stab}{Output of \code{stability()}.}
#'   \item{plaus}{Output of \code{plausible_models()}.}
#' }
#'
#' @examples
#' ## Gaussian Model Example
#' set.seed(123)
#' n <- 120; p <- 6
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' beta <- c(2, -1, 0, 0, 1, 0)
#' y <- as.numeric(as.matrix(X) %*% beta + rnorm(n))
#'
#' result <- multipath_aic(
#'   X, y,
#'   family = "gaussian",
#'   K = 6,
#'   B = 20,
#'   Delta = 2,
#'   tau = 0.6,
#'   verbose = FALSE
#' )
#'
#' ## Inspect plausible models
#' result$plaus$summary
#'
#' ## Logistic Model Example
#' set.seed(1)
#' n <- 150; p <- 6
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' eta <- 1.5*X$x1 - 2*X$x2 + 1*X$x5
#' prob <- 1 / (1 + exp(-eta))
#' y <- rbinom(n, 1, prob)
#'
#' result <- multipath_aic(
#'   X, y,
#'   family = "binomial",
#'   K = 6,
#'   B = 20,
#'   verbose = FALSE
#' )
#'
#' @export
multipath_aic <- function(
    X, y,
    family = c("gaussian", "binomial"),
    K = min(ncol(X), 10),
    eps = 1e-6,
    delta = 2,
    L = 100,
    B = 100,
    resample_fraction = 0.8,
    Delta = 2,
    tau = 0.6,
    verbose = TRUE
) {
  family <- match.arg(family)
  if (verbose) cat("========== Overall Multi-Path AIC Procedure ==========\n")

  # Step 1: Multi-Path Forward Selection
  if (verbose) cat("\n[Step 1] Running Multi-Path Forward Selection...\n")
  forest <- build_paths(
    X, y,
    family = family,
    K = K,
    eps = eps,
    delta = delta,
    L = L,
    verbose = verbose
  )

  # Step 2: Stability Estimation with Resampling
  if (verbose) cat("\n[Step 2] Estimating Variable Stability with Resampling...\n")
  stab <- stability(
    X, y,
    family = family,
    K = K,
    eps = eps,
    delta = delta,
    L = L,
    B = B,
    resample_fraction = resample_fraction,
    verbose = verbose
  )

  # Step 3: Selecting Plausible Models
  if (verbose) cat("\n[Step 3] Selecting Plausible Stable Models...\n")
  plaus <- plausible_models(
    forest = forest,
    pi = stab$pi,
    Delta = Delta,
    tau = tau,
    verbose = verbose
  )

  # Step 4: Output Final Combined Result
  if (verbose) cat("\n[Step 4] Final Output: Plausible models and stability summary.\n")
  if (verbose) cat("==================================================================\n")

  # Attach data for downstream evaluation
  forest$model_data <- list(X = X, y = y)

  return(list(
    forest = forest,
    stab = stab,
    plaus = plaus
  ))
}


#' Confusion Matrix and Diagnostic Metrics for Selected Logistic Models
#'
#' @description
#' Computes a confusion matrix and several standard diagnostic metrics for a
#' **logistic regression model** selected by the Multi-Path AIC procedure.
#'
#' This function extracts a model from the set of plausible models produced by
#' [`multipath_aic()`], refits the logistic regression on the original
#' data, and evaluates its predictive performance at a user-specified cutoff.
#'
#' @param result_object The full output list returned by
#'   [`multipath_aic()`]. Must contain `forest` and `plaus`.
#' @param model_index Integer index specifying which plausible model to evaluate.
#'   Default is `1` (the top plausible model).
#' @param cutoff Probability threshold for classification. Default: `0.5`.
#' @param verbose Logical; if TRUE, prints the confusion matrix and metrics.
#'
#' @details
#' The metrics computed include:
#'
#' \itemize{
#'   \item \strong{Prevalence} — proportion of positive outcomes.
#'   \item \strong{Accuracy} — proportion of correct classifications.
#'   \item \strong{Sensitivity (Recall)} — \(TP / (TP + FN)\).
#'   \item \strong{Specificity} — \(TN / (TN + FP)\).
#'   \item \strong{False Discovery Rate (FDR)} — \(FP / (TP + FP)\).
#'   \item \strong{Diagnostic Odds Ratio (DOR)} — \((TP/FN) / (FP/TN)\).
#' }
#'
#' @return
#' A data frame containing:
#' \describe{
#'   \item{Prevalence}{Proportion of true positives.}
#'   \item{Accuracy}{Correct classification rate.}
#'   \item{Sensitivity}{True positive rate.}
#'   \item{Specificity}{True negative rate.}
#'   \item{FDR}{False discovery rate.}
#'   \item{DOR}{Diagnostic odds ratio.}
#' }
#'
#' @examples
#' ## Logistic Model Evaluation Example
#' set.seed(1)
#' n <- 150; p <- 6
#' X <- as.data.frame(matrix(rnorm(n*p), n, p))
#' names(X) <- paste0("x", 1:p)
#' eta <- 1.5*X$x1 - 2*X$x2 + 1*X$x5
#' prob <- 1 / (1 + exp(-eta))
#' y <- rbinom(n, 1, prob)
#'
#' result <- multipath_aic(
#'   X, y,
#'   family = "binomial",
#'   K = 6,
#'   B = 20,
#'   verbose = FALSE
#' )
#'
#' # Evaluate first plausible model
#' confusion_metrics(result, model_index = 1)
#'
#' @export
confusion_metrics <- function(result_object, model_index = 1, cutoff = 0.5, verbose = TRUE) {
  # --- Input checks ---
  if (is.null(result_object$plaus$plausible_models)) {
    stop("Result object must come from multipath_aic().")
  }

  plausible_set <- result_object$plaus$plausible_models
  if (nrow(plausible_set) < model_index) {
    stop("Requested model_index exceeds number of plausible models.")
  }

  # --- Identify model ID ---
  model_id <- rownames(plausible_set)[model_index]
  if (is.null(model_id) || model_id == "") model_id <- model_index

  # --- Extract selected variables ---
  selected_vars <- plausible_set$model[[model_index]]
  if (length(selected_vars) == 0) stop("Selected model has no predictors.")

  # --- Retrieve X and y from stored data ---
  X <- result_object$forest$model_data$X
  y <- result_object$forest$model_data$y

  # --- Refit logistic model ---
  df <- data.frame(y = y, X)
  form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
  model <- glm(form, data = df, family = binomial())

  # --- Predictions ---
  p_hat <- predict(model, type = "response")
  y_pred <- ifelse(p_hat >= cutoff, 1, 0)
  y_true <- y

  # --- Confusion matrix components ---
  TP <- sum(y_pred == 1 & y_true == 1)
  TN <- sum(y_pred == 0 & y_true == 0)
  FP <- sum(y_pred == 1 & y_true == 0)
  FN <- sum(y_pred == 0 & y_true == 1)
  eps <- 1e-8

  # --- Metrics ---
  prevalence  <- mean(y_true == 1)
  accuracy    <- (TP + TN) / (TP + TN + FP + FN + eps)
  sensitivity <- TP / (TP + FN + eps)
  specificity <- TN / (TN + FP + eps)
  FDR         <- FP / (TP + FP + eps)
  DOR         <- (TP / (FN + eps)) / (FP / (TN + eps))

  metrics <- data.frame(
    Prevalence  = round(prevalence, 3),
    Accuracy    = round(accuracy, 3),
    Sensitivity = round(sensitivity, 3),
    Specificity = round(specificity, 3),
    FDR         = round(FDR, 3),
    DOR         = round(DOR, 3)
  )

  if (verbose) {
    cat("\n========== Confusion Metrics for Model ID", model_id, "==========\n")
    cat("Variables:", paste(selected_vars, collapse = ", "), "\n\n")
    cat("Confusion Matrix (cutoff =", cutoff, "):\n")
    print(matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                 dimnames = list("Predicted" = c("1","0"),
                                 "Actual"    = c("1","0"))))
    cat("\nMetrics:\n")
    print(metrics)
  }

  invisible(metrics)
}

