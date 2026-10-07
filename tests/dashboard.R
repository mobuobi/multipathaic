optional <- c("shiny", "ggplot2", "plotly", "DT", "shinydashboard", "htmltools")
if (all(vapply(optional, requireNamespace, logical(1), quietly = TRUE))) {
  app <- new.env()
  source(system.file("RS_int", "app.R", package = "multipathaic"), local = app)
  shiny::testServer(app$server, {
    stopifnot(roc_summary(c(0, 1), c(0.0001, 0.0002))$auc == 1,
              roc_summary(c(0, 1), c(0.9, 0.1))$auc == 0,
              roc_summary(c(0, 1), c(0.5, 0.5))$auc == 0.5)
    session$setInputs(data_source = "synthetic", family = "gaussian", upload_family = "gaussian",
      n_obs = 80, n_pred = 4, K = 3, B = 3, delta = 2, L = 10, Delta = 2,
      tau = 0, analysis_seed = 123, train_pct = 70, add_interactions = FALSE)
    session$setInputs(generate_data = 1)
    stopifnot(nrow(rv$X) == 80)
    session$setInputs(apply_features = 1)
    stopifnot(nrow(rv$X_train) == 56, nrow(rv$X_test) == 24)
    session$setInputs(run = 1)
    stopifnot(!is.null(rv$result), rv$run_params$seed == 123)
    html <- output$test_eval_content$html
    stopifnot(grepl("Test R²", html), grepl("<table", html, fixed = TRUE))
    # Independently calculate the displayed held-out coefficient of determination.
    vars <- rv$result$plaus$plausible_models$model[[1]]
    ref <- lm(y ~ ., data.frame(y = rv$y_train, rv$X_train[, vars, drop = FALSE]))
    pred <- predict(ref, newdata = rv$X_test[, vars, drop = FALSE])
    r2 <- 1 - sum((rv$y_test - pred)^2) / sum((rv$y_test - mean(rv$y_test))^2)
    stopifnot(grepl(as.character(round(r2, 3)), html, fixed = TRUE))
    before <- rv$result$stab$pi
    session$setInputs(run = 2)
    stopifnot(identical(before, rv$result$stab$pi))
    session$setInputs(tau = 0.95)
    stopifnot(rv$run_params$tau == 0)
    # Download uses the completed run's data and settings, not current sliders.
    report <- paste(readLines(output$download_report), collapse = "\n")
    stopifnot(grepl("56</li>", report, fixed = TRUE), grepl("<strong>τ (Stability):</strong> 0</li>", report, fixed = TRUE))
    session$setInputs(generate_data = 2)
    stopifnot(is.null(rv$result), is.null(rv$X_train), is.null(rv$X_test))
    # Empty and intercept-only results render and export without indexing errors.
    session$setInputs(K = 0, tau = 0.6, run = 3)
    stopifnot(nrow(rv$result$plaus$plausible_models) == 1)
    invisible(output$models_by_step); invisible(output$branching_tree)
    invisible(output$plausible_table); invisible(output$inclusion_plot)
    rv$result$plaus$plausible_models <- rv$result$plaus$plausible_models[FALSE, ]
    rv$result$plaus$inclusion <- numeric()
    session$flushReact()
    invisible(output$plausible_table)
    stopifnot(file.exists(output$download_report))
    # Family change invalidates prepared data. Generate and run a logistic split.
    session$setInputs(family = "binomial")
    stopifnot(is.null(rv$X), is.null(rv$result))
    session$setInputs(generate_data = 3, K = 2, tau = 0)
    session$setInputs(apply_features = 2)
    stopifnot(length(unique(rv$y_train)) == 2, length(unique(rv$y_test)) == 2)
    session$setInputs(run = 4)
    stopifnot(!is.null(rv$result))
    invisible(output$confusion_plot); invisible(output$roc_curve)
    invisible(output$metrics_boxes); invisible(output$test_eval_content)
    # Response leakage and implicit category order are rejected on upload.
    session$setInputs(data_source = "upload", upload_family = "gaussian")
    rv$data <- data.frame(response = 1:6, category = letters[1:6], predictor = 6:1)
    session$setInputs(response_var = "response", predictor_vars = c("response", "predictor"))
    session$setInputs(confirm_vars = 1)
    stopifnot(is.null(rv$X))
    session$setInputs(predictor_vars = "category", confirm_vars = 2)
    stopifnot(is.null(rv$X))
    # Complete cases are selected jointly, with names retained.
    rv$data$predictor[2] <- NA_real_
    session$setInputs(predictor_vars = "predictor", confirm_vars = 3)
    stopifnot(nrow(rv$X) == 5, identical(rv$y, c(1L, 3L, 4L, 5L, 6L)))
  })
  cat("Dashboard regression checks passed.\n")
} else {
  cat("Dashboard checks skipped: optional app dependencies are not installed.\n")
}
