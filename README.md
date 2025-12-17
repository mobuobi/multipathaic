# multipathaic

Multi-path AIC selection for linear and logistic regression with stability-based model filtering.

## Quick Links

- **[Function Reference](function_reference.md)** - Complete API documentation
- **[Method Vignette](vignettes/method_vignette.Rmd)** - Detailed methodology
- **[Diabetes Analysis](vignettes/diabetes-analysis.Rmd)** - Case study demonstration


## Installation

Install the package directly from GitHub:
```r
# Install remotes if not already installed
install.packages("remotes")

# Install multipathaic
remotes::install_github("mobuobi/multipathaic")
```

## Interactive Shiny App 

Explore the multi-path AIC selection procedure with our comprehensive interactive dashboard!

### Launch the App
```r
# Install required packages first (if not already installed)
install.packages(c("shiny", "shinydashboard", "plotly", "DT", "ggplot2", "readxl"))

# Load package and launch
library(multipathaic)
launch_app()
```

###  App Features

#### **Data Management**
- **Flexible Data Input**:
  - Upload your own data files (CSV, TXT, XLS, XLSX)
  - Select specific sheets from Excel workbooks
  - Configure delimiters for text files (comma, semicolon, tab, space)
  - Generate synthetic data for testing and demonstrations
  
- **Data Cleaning & Preview**:
  - Automatic handling of missing values
  - Column name sanitization
  - Interactive data preview with filtering
  - Comprehensive data summary statistics

#### **Algorithm Controls**
- **Real-time Parameter Tuning**:
  - K: Maximum forward selection steps (3-20)
  - δ: AIC branching tolerance (0-5)
  - L: Maximum models per step (10-200)
  - B: Bootstrap resamples (10-200)
  - Δ: Plausibility tolerance (0-5)
  - τ: Stability threshold (0-1)

#### **Visualizations**

**Results Tab:**
- Variable stability scores with threshold line
- Models retained by step (line plot with area fill)
- Top 10 most stable variables with color-coded bars
- Summary value boxes (steps, models, AIC, runtime)

**Plausible Models Tab:**
- Interactive table of selected models with AIC and stability
- Jaccard overlap heatmap between models
- Variable inclusion probability across plausible models

**Branching Tree Tab:**
- Interactive scatter plot of all explored models
- Step-by-step visualization of path exploration
- Point size indicates number of variables
- Color-coded by selection step

**Diagnostics Tab:**

*For Binomial (Logistic) Models:*
- Confusion matrix with color gradient (TP, TN, FP, FN)
- Performance metric cards (Accuracy, Sensitivity, Specificity, Precision)
- ROC curve with AUC calculation
- Predicted probability distribution by class

*For Gaussian (Linear) Models:*
- Residual plot with LOESS smoothing
- Predicted vs Actual scatter with R² and RMSE
- Model performance summary cards

#### **Export & Reporting**
- Download comprehensive HTML reports
- Model summaries with parameter settings
- Top stable variables table
- Plausible models ranking

### App Screenshots

The app features a modern dashboard layout with:
-  Blue-themed professional interface
-  Responsive design with collapsible sections
-  Interactive plots powered by Plotly
-  Sortable, filterable data tables
-  Value boxes for key metrics
-  Real-time progress indicators

###  Educational Use

Perfect for:
- Teaching variable selection methods
- Demonstrating bootstrap stability
- Comparing model selection strategies
- Understanding AIC-based selection
- Exploring multi-path algorithms

###  Example Workflow in App

1. **Upload Data**: Navigate to Data tab, upload your CSV/Excel file
2. **Preview**: Review data summary and select variables
3. **Set Parameters**: Go to Parameters tab, adjust K, δ, Δ, τ, B
4. **Run Analysis**: Click "Run Multi-Path AIC Analysis"
5. **View Results**: Explore stability plots and value boxes
6. **Check Models**: Review plausible models and overlap
7. **Explore Paths**: Visualize branching in Branching Tree tab
8. **Diagnostics**: Examine performance metrics and plots
9. **Download**: Export HTML report of findings

## Quick Start

### Linear Regression Example (Gaussian)
```r
library(multipathaic)

# Generate synthetic data
set.seed(123)
n <- 150; p <- 10
X <- as.data.frame(matrix(rnorm(n*p), n, p))
names(X) <- paste0("x", 1:p)
beta <- c(2, -1.5, 0, 0, 1, rep(0, p-5))
y <- as.numeric(as.matrix(X) %*% beta + rnorm(n, 1))

# Step 1: Build multi-path forest
forest <- build_paths(
  X = X, 
  y = y, 
  family = "gaussian",
  K = 10,
  eps = 1e-6,
  delta = 2,
  L = 50
)

# Step 2: Compute stability across resamples
stab <- stability(
  X = X,
  y = y,
  family = "gaussian",
  B = 50,
  K = 10,
  eps = 1e-6,
  delta = 2,
  L = 50
)

# Step 3: Select plausible models
plaus <- plausible_models(
  forest = forest,
  pi = stab$pi,
  Delta = 2,
  tau = 0.6
)

# View results
print(plaus$summary)
```

**Or use the all-in-one wrapper:**
```r
# Complete pipeline in one function
result <- multipath_aic(
  X = X,
  y = y,
  family = "gaussian",
  K = 10,
  B = 50,
  Delta = 2,
  tau = 0.6
)

# Access components
result$forest        # Multi-path search results
result$stab          # Stability scores
result$plaus         # Plausible models
```

### Logistic Regression Example (Binomial)
```r
library(multipathaic)

# Generate binary outcome data
set.seed(42)
n <- 200; p <- 8
X <- as.data.frame(matrix(rnorm(n*p), n, p))
names(X) <- paste0("x", 1:p)

# True model: x1, x2, x5 are important
eta <- 1.5*X$x1 - 2*X$x2 + 1*X$x5
prob <- 1 / (1 + exp(-eta))
y <- rbinom(n, 1, prob)

# Step 1: Build multi-path forest
forest <- build_paths(
  X = X,
  y = y,
  family = "binomial",
  K = 8,
  eps = 1e-6,
  delta = 2,
  L = 50
)

# Step 2: Compute stability
stab <- stability(
  X = X,
  y = y,
  family = "binomial",
  B = 50,
  K = 8,
  eps = 1e-6,
  delta = 2,
  L = 50
)

# Step 3: Select plausible models
plaus <- plausible_models(
  forest = forest,
  pi = stab$pi,
  Delta = 2,
  tau = 0.6
)

# View plausible models
print(plaus$plausible_models)
```

**Evaluate classification performance:**
```r
# Run complete pipeline
result <- multipath_aic(
  X = X,
  y = y,
  family = "binomial",
  K = 8,
  B = 50,
  Delta = 2,
  tau = 0.6
)

# Compute confusion matrix and metrics for best model
confusion_metrics(result, model_index = 1, cutoff = 0.5)
```

## Parameters

### Core Parameters

- **`K`**: Maximum number of forward selection steps (default: `min(ncol(X), 10)`)
- **`eps`**: Minimum AIC improvement threshold (default: `1e-6`)
- **`delta`**: AIC tolerance for keeping near-best models at each step (default: `2`)
- **`L`**: Maximum models retained per step (default: `100`)

### Stability Parameters

- **`B`**: Number of bootstrap resamples (default: `100`)
- **`resample_fraction`**: Fraction of data per resample (default: `0.8`)

### Plausibility Parameters

- **`Delta`**: AIC tolerance for plausible model set (default: `2`)  
  *Justification: Models within 2 AIC units are considered statistically equivalent (Burnham & Anderson, 2002)*
- **`tau`**: Minimum average stability threshold (default: `0.6`)  
  *Justification: Retains models with variables appearing in >60% of resamples, indicating robust selection*

## Real Data Example

The package includes a detailed vignette using the diabetes progression dataset:
```r
# View the vignette
vignette("diabetes-analysis", package = "multipathaic")
```

## Key Functions

| Function | Description |
|----------|-------------|
| `build_paths()` | Multi-path forward selection with AIC branching |
| `stability()` | Bootstrap-based variable stability estimation |
| `plausible_models()` | Filter models by AIC + stability |
| `multipath_aic()` | Complete pipeline (Algorithms 1-3) |
| `confusion_metrics()` | Classification performance metrics |
| `launch_app()` | Launch interactive Shiny application |

## Workflow Diagram
```
Data (X, y)
    ↓
┌─────────────────────────────────────┐
│  Algorithm 1: build_paths()         │
│  Multi-path forward selection       │
│  - Explores multiple branches       │
│  - Keeps near-optimal models        │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  Algorithm 2: stability()           │
│  Bootstrap resampling               │
│  - Computes variable stability      │
│  - Identifies reliable predictors   │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  Algorithm 3: plausible_models()    │
│  AIC + stability filtering          │
│  - Combines fit & stability         │
│  - Returns final model set          │
└─────────────────────────────────────┘
    ↓
Final Plausible Models
```

## System Requirements

### Required R Packages
```r
# Core dependencies (automatically installed)
- stats

# Shiny app dependencies (install manually)
install.packages(c("shiny", "shinydashboard", "plotly", "DT", "ggplot2"))

# For Excel file support (optional)
install.packages("readxl")

# For vignette datasets (optional)
install.packages(c("lars", "caret"))
```

## Citation

If you use this package, please cite:
```
Obuobi, M. (2025). multipathaic: Multi-Path Stepwise 
Selection with AIC. R package version 0.1.0. 
https://github.com/mobuobi/multipathaic.git
```

## References

- Burnham, K. P., & Anderson, D. R. (2002). *Model Selection and Multimodel Inference: A Practical Information-Theoretic Approach* (2nd ed.). Springer.
- Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004). Least angle regression. *The Annals of Statistics*, 32(2), 407-499.

## Author

- **Michael Obuobi** - [obuobimichael25@gmail.com](mailto:obuobimichael25@gmail.com)

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Issues and Support

- **Report bugs**: https://github.com/mobuobi/multipathaic/issues
- **Request features**: https://github.com/mobuobi/multipathaic/issues
- **View documentation**: `?multipathaic` or `help(package = "multipathaic")`

---

**Package Status**: Active Development  

## Acknowledgments

I thank the Auburn University Department of Mathematics and Statistics for support and guidance throughout this project.

