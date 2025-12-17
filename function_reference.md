# Function Reference

Quick reference guide for all `multipathaic` package functions.

---

## Core Functions

### `multipath_aic()`

**Complete pipeline for multi-path AIC selection**

Runs the full three-stage procedure: path building, stability estimation, and plausible model filtering.

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `X` | data.frame/matrix | - | Predictor matrix (n × p) |
| `y` | numeric vector | - | Response vector (length n) |
| `family` | character | - | Model family: "gaussian" or "binomial" |
| `K` | integer | `min(ncol(X), 20)` | Maximum forward selection steps |
| `eps` | numeric | `1e-6` | Minimum AIC improvement threshold |
| `delta` | numeric | `2` | AIC tolerance for branching |
| `L` | integer | `50` | Maximum models retained per level |
| `B` | integer | `50` | Number of bootstrap resamples |
| `resample_fraction` | numeric | `0.8` | Bootstrap sample fraction (0-1) |
| `Delta` | numeric | `2` | AIC tolerance for plausibility filter |
| `tau` | numeric | `0.6` | Minimum stability threshold (0-1) |
| `verbose` | logical | `TRUE` | Print progress messages |

#### Returns

List containing:

- `forest`: Output from `build_paths()`
- `stability`: Output from `stability()`
- `plausible_models`: Output from `plausible_models()`

#### Example
```r
library(multipathaic)
data(diabetes, package = "lars")

result <- multipath_aic(
  X = diabetes$x,
  y = diabetes$y,
  family = "gaussian",
  K = 10,
  delta = 2,
  L = 50,
  B = 50,
  tau = 0.6
)

# View plausible models
print(result$plausible_models$plausible_models)
```

---

### `build_paths()`

**Multi-path forward selection with AIC-based branching**

Explores multiple competitive model paths simultaneously by keeping near-optimal models at each step.

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `X` | data.frame/matrix | - | Predictor matrix |
| `y` | numeric vector | - | Response vector |
| `family` | character | - | "gaussian" or "binomial" |
| `K` | integer | `min(ncol(X), 20)` | Maximum forward steps |
| `eps` | numeric | `1e-6` | Minimum AIC improvement |
| `delta` | numeric | `2` | AIC tolerance for branching |
| `L` | integer | `50` | Maximum models per level |
| `verbose` | logical | `TRUE` | Print progress |

#### Returns

List containing:

- `all_models`: data.frame with all explored models (step, variables, AIC)
- `path_forest`: List structure with frontiers at each step

#### Example
```r
forest <- build_paths(
  X = X_train,
  y = y_train,
  family = "gaussian",
  K = 10,
  delta = 2,
  L = 50
)

# Total models explored
nrow(forest$all_models)

# Best AIC at final step
min(forest$all_models$AIC)
```

---

### `stability()`

**Bootstrap-based variable stability estimation**

Computes selection frequency for each predictor across bootstrap resamples to identify stable variables.

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `X` | data.frame/matrix | - | Predictor matrix |
| `y` | numeric vector | - | Response vector |
| `family` | character | - | "gaussian" or "binomial" |
| `K` | integer | `min(ncol(X), 20)` | Maximum steps |
| `eps` | numeric | `1e-6` | Min AIC improvement |
| `delta` | numeric | `2` | AIC branching tolerance |
| `L` | integer | `50` | Max models per level |
| `B` | integer | `50` | Number of bootstrap samples |
| `resample_fraction` | numeric | `0.8` | Bootstrap sample size (0-1) |
| `verbose` | logical | `TRUE` | Print progress |

#### Returns

List containing:

- `pi`: Named numeric vector of stability scores (0-1) for each variable

#### Example
```r
stab <- stability(
  X = X_train,
  y = y_train,
  family = "gaussian",
  K = 10,
  B = 100,
  resample_fraction = 0.8
)

# Top 10 most stable variables
head(stab$pi, 10)

# Variables with stability > 0.6
names(stab$pi[stab$pi > 0.6])
```

---

### `plausible_models()`

**Filter models by AIC and stability criteria**

Identifies plausible models that are both near-optimal (low AIC) and built from stable variables.

#### Arguments

| Argument | Type | Default | Description |
|----------|------|---------|-------------|
| `forest` | list | - | Output from `build_paths()` |
| `pi` | named numeric | - | Stability scores from `stability()` |
| `Delta` | numeric | `2` | AIC tolerance from best |
| `tau` | numeric | `0.6` | Minimum average stability |
| `verbose` | logical | `TRUE` | Print progress |

#### Returns

List containing:

- `plausible_models`: data.frame of models passing filters
- `best_aic`: Numeric, lowest AIC value
- `summary`: data.frame with variable inclusion frequencies and mean stability

#### Example
```r
plaus <- plausible_models(
  forest = forest,
  pi = stab$pi,
  Delta = 2,
  tau = 0.6
)

# Number of plausible models
nrow(plaus$plausible_models)

# Variables in best model
plaus$plausible_models$model[[1]]

# Variable summary
head(plaus$summary, 10)
```

---

## Parameter Guidelines

### Quick Reference Table

| Parameter | Typical Range | Recommended Default | Purpose |
|-----------|---------------|---------------------|---------|
| **K** | 5-20 | `min(p, 10)` | Limits model complexity |
| **eps** | 1e-8 to 1e-4 | `1e-6` | Controls forward step sensitivity |
| **delta** | 1-4 | `2` | Path exploration breadth |
| **L** | 25-100 | `50` | Computational vs exploration tradeoff |
| **B** | 50-200 | `100` | Stability precision |
| **resample_fraction** | 0.6-0.9 | `0.8` | Bootstrap sample size |
| **Delta** | 1-4 | `2` | AIC plausibility window |
| **tau** | 0.5-0.8 | `0.6` | Stability threshold |

### Guidance by Use Case

**Small datasets (n < 100)**

- Reduce `B = 30-50` (fewer bootstrap samples)
- Increase `resample_fraction = 0.9`
- Consider `L = 25` (fewer paths)

**High-dimensional (p > n)**

- Limit `K = 5-8` to prevent overfitting
- Increase `tau = 0.7-0.8` for stricter stability
- Use `L = 100` for thorough exploration

**Large datasets (n > 1000)**

- Increase `B = 100-200` for precise stability
- Can explore more: `L = 75-100`, `K = 15-20`

**Highly correlated predictors**

- Use wider tolerance: `delta = 3-4`
- Stability becomes critical: ensure `B >= 100`, `tau >= 0.6`

---

## Statistical Background

### AIC Tolerance (delta, Delta)

Models within **2 AIC units** are considered to have essentially equivalent empirical support (Burnham & Anderson, 2002). This is a widely-accepted guideline:

- ΔAICᵢ < 2: Substantial support
- 2 ≤ ΔAICᵢ ≤ 7: Considerably less support
- ΔAICᵢ > 10: Essentially no support

**Recommendation:** Use `delta = Delta = 2` unless you have specific reasons to be more/less conservative.

### Stability Threshold (tau)

Higher values require variables to appear more consistently across resamples:

- **tau = 0.5**: Variable appears in >50% of bootstrap samples (moderate)
- **tau = 0.6**: Variable appears in >60% of samples (recommended)
- **tau = 0.7**: Variable appears in >70% of samples (conservative)

**Recommendation:** Start with `tau = 0.6`. Increase to 0.7-0.8 if you need more reliable variable selection; decrease to 0.5 if no models pass the filter.

### Bootstrap Samples (B)

More samples = more precise stability estimates but longer computation:

- **B = 50**: Fast, reasonable for exploratory analysis
- **B = 100**: Recommended default (good precision/speed balance)
- **B = 200**: High precision for final analyses

**Recommendation:** Use `B = 100` for most applications.

---

## References

Burnham, K. P., & Anderson, D. R. (2002). *Model Selection and Multimodel Inference: A Practical Information-Theoretic Approach* (2nd ed.). Springer-Verlag.

---

**Last Updated:** December 2025  
**Package Version:** 0.1.0  
**Maintainer:** Michael Obuobi
