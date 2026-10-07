# multipathaic 0.1.1

- Correct per-child AIC improvement filtering and cache repeated model fits.
- Validate inputs and exclude invalid fits; preserve safe predictor identities.
- Correct bootstrap success accounting and report failed draws.
- Handle null and empty model sets and undefined diagnostic metrics.
- Repair dashboard data/split invalidation, confusion-matrix placement, ROC integration,
  test R², reproducibility, reports, and narrow-window layouts.
- Remove test-data leakage from the diabetes example and align methodology documentation.
- Add regression tests and automated R package checks.

Results may change because of the corrected search and bootstrap accounting.
`stability()$B` now reports successful draws; `B_requested` records attempted draws.
The core API requires finite numeric predictors. Intercept-only models are exempt from
variable-stability filtering and have `NA` average stability.
