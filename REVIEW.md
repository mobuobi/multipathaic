# Review of multipathaic

Review started on 2026-09-21; final validation completed on 2026-10-07 against GitHub commit `9d8ada3c69ac380f39d2d11088df42bce968f366`.
The revised package version is 0.1.1. Changes are intended for review before release.

## Statistical findings and corrections

| Finding | Consequence | Correction |
| --- | --- | --- |
| A parent expanded when its best child improved, but other retained children did not individually need to improve. | Paths could move toward worse AIC despite the documented rule. | Require each child to pass both its parent improvement threshold and its parent's near-best threshold. |
| Missing inputs were handled implicitly by each model fit. | Different candidates could use different observations, invalidating the intended AIC comparison. | Core API rejects non-finite inputs; the dashboard drops rows jointly across selected variables and reports the count. |
| Failed bootstrap draws were included in the denominator. | Failures lowered scores as if they were valid draws selecting no predictors. | Count successful draws, report failures, and stop if every draw fails. Valid null-only draws still contribute zeros. |
| The method vignette described a best-model frequency, global child filtering, and an incorrectly signed stopping rule. | Published pseudocode disagreed with the implementation. | Align the description with per-parent filtering and average inclusion across the deepest retained frontier. |
| The diabetes vignette screened interactions on all outcomes and standardized before splitting. | Test observations influenced model development. | Split first; screen and estimate scaling using training observations only. Replace the otherwise unused caret dependency with a base-R random split. |
| AIC proximity and tau were described too strongly. | Readers could infer statistical equivalence or variable-level reliability guarantees. | Explain relative AIC screening, model-average filtering, and descriptive stability. |
| Intercept-only results could crash summary creation or be automatically removed by the stability threshold. | Null selections were not represented consistently. | Keep eligible null models with NA average stability; exempt them from the variable-stability filter. Empty sets return correctly shaped tables. |
| Test R² was squared correlation. | It could obscure poorly calibrated held-out predictions. | Use 1 − SSE/SST, with NA for constant test outcomes. |

R's AIC documentation requires comparable maximum-likelihood fits on the same data;
this is the basis for rejecting model-specific missing-value omission. See the
[R AIC reference](https://www.stat.ethz.ch/R-manual/R-devel/library/stats/html/AIC.html)
and [GLM reference](https://stat.ethz.ch/R-manual/R-devel/library/stats/help/glm.html).

The present bootstrap procedure differs from the subsampling-based procedure and
assumptions used to obtain theoretical stability-selection error bounds. This
review does **not** establish those bounds for multipathaic. See
[Meinshausen and Bühlmann, Stability Selection](https://arxiv.org/abs/0809.2932).

## Code and package improvements

- Validate dimensions, numeric finite values, unique predictor names, binary outcomes,
  weights, step limits, tolerances, and bootstrap settings.
- Internally rename model columns so predictor names such as `y`, spaces, `%`, or `|`
  cannot collide with the response, formula syntax, or model keys. The variable list
  in each model is authoritative.
- Cache AIC by variable set, reuse numeric model matrices, and assemble candidate tables
  per parent to avoid duplicate fits and repeated formula/data-frame overhead. Compare
  optimized results against direct R lm/glm AIC, including zero-weight observations.
- Reject invalid, non-finite, saturated, or nonconverged/boundary logistic candidate
  fits and report their count. This is not a complete separation diagnostic.
- Correct confusion-metric input checks; allow null models; return NA for undefined
  ratios and Inf for a positive diagnostic odds ratio with zero denominator.
- Declare optional dashboard/vignette dependencies, check app dependencies before
  launch, normalize the incorrectly named `LICENSE ` file to `LICENSE`, remove
  nonportable code characters, and exclude development files from source builds.
- Add core and Shiny regression tests and a GitHub Actions package-check workflow.

## Dashboard improvements

- Invalidate prepared data, old splits, and results when data or confirmed selections
  change; reject use of the response as a predictor.
- Reject implicit numeric ordering of categorical predictors. Require explicit
  numeric encoding; display the 0/1 mapping for categorical binary responses.
- Stratify binary train/test splits, guard single-predictor interaction requests,
  and create unique feature names.
- Add a bootstrap seed and save analysis settings with each result. Show successful
  versus attempted resamples. Reports use the completed run's parameters and data.
- Label diagnostic plots as training performance and keep held-out evaluation separate.
- Correct false-positive/false-negative placement in the confusion heatmap; use every
  distinct fitted probability and endpoints for ROC integration; handle undefined ratios.
- Handle empty/null model displays and reports; escape report variable labels;
  fix the report repository link; avoid installing packages from inside the app.
- Improve card and panel layout in narrow windows and suppress inappropriate residual
  smoothing for constant fitted values.

## Validation

Final result: **R CMD check — Status: OK (0 errors, 0 warnings, 0 notes)** on
R 4.5.3 / macOS arm64. Both regression suites, all package examples, and the R code
in both vignettes passed. The source archive contains both rendered HTML vignettes.
The final check reused those rendered files instead of rebuilding their HTML.

```sh
R CMD check --no-manual --no-build-vignettes multipathaic_0.1.1.tar.gz
```

The local check log is `validation/check.log`. The `lars` dependency is installed
in the workspace's `.r-library` for validation. Browser verification covered a
completed Gaussian analysis, results and diagnostics navigation, and narrow-window
layout. Server regression tests additionally cover binomial analyses. GitHub Actions
has been configured; its results are separate from this local validation.
The `tests/regression.R` suite covers direct R AIC agreement, parent improvement,
weighted fits, null/empty results, unusual names, invalid inputs, reproducibility,
bootstrap failure accounting, and logistic metrics. `tests/dashboard.R` uses
`shiny::testServer` for Gaussian and binomial runs, split invalidation, held-out R²,
repeatability, saved report settings, null/empty output, and upload validation.

A small reproducible sanity study is in `validation/simulation.R`; raw outputs are
in `validation/simulation-results.csv`. It uses five seeds in each of four scenarios,
120 observations, six predictors, 84 training rows, and 20 bootstrap draws. Both
Gaussian signal variables had mean stability 1.0; logistic signal scores averaged
1.0 and 0.95. Noise scores remained nonzero, and a null scenario could retain noise
variables or return an empty plausible set. This is a smoke study, not evidence of
uniform recovery, predictive superiority, or error-rate control.

## Remaining methodological limits

1. Forward search with a beam cap is not exhaustive; suppressor effects and jointly
   useful predictors can be missed. AIC is optimized only among retained candidates.
2. Stability currently uses only the deepest frontier, excluding shorter terminal
   branches. This definition is preserved and documented; changing it needs a separate
   methodological decision and simulation comparison.
3. Correlated variables can substitute for one another. Increasing bootstrap draws
   reduces Monte Carlo noise but does not guarantee identification of the true model.
4. Frequent resampling failures condition scores on the successful subset and can bias
   interpretation. Inspect failures; do not treat a small success count as sufficient.
5. Mean model stability can hide a low-scoring variable. Inclusion fractions are neither
   posterior probabilities nor evidence of causality. No false-discovery bound is claimed.
6. Interaction selection does not enforce hierarchy. Categorical predictors need explicit
   encoding, and dummy columns are selected separately rather than as grouped terms.
7. Training diagnostics are optimistic after selection. Perform all learned preprocessing,
   selection, and tuning inside training folds; reserve untouched data for final evaluation.
8. Ordinary GLMs remain vulnerable to separation and rank deficiency. Finite converged
   fits are not a proof that all regularity conditions for AIC hold. Penalized/Firth fits
   would need a separate design for a comparable information criterion.
