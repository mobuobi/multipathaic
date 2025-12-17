#
# Multi-Path AIC Selection - Enhanced Interactive Shiny App
# Demonstrates the multipathaic package with visualizations and data upload
# Supports CSV, TXT, XLS, and XLSX files
#

library(shiny)
library(multipathaic)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)

# UI Definition
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Multi-Path AIC Explorer",
    titleWidth = 300
  ),

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Parameters", tabName = "parameters", icon = icon("sliders")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Plausible Models", tabName = "plausible", icon = icon("bullseye")),
      menuItem("Branching Tree", tabName = "branching", icon = icon("code-branch")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("microscope")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#3c8dbc
        }
        .box.box-solid.box-success>.box-header {
          color:#fff;
          background:#00a65a
        }
        .box.box-solid.box-warning>.box-header {
          color:#fff;
          background:#f39c12
        }
        .box.box-solid.box-danger>.box-header {
          color:#fff;
          background:#dd4b39
        }
        .box.box-solid.box-info>.box-header {
          color:#fff;
          background:#00c0ef
        }
        .small-box {
          border-radius: 10px;
        }
        .info-box {
          min-height: 90px;
        }
      "))
    ),

    tabItems(
      # ==================== Data Tab ====================
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Data Source",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            radioButtons("data_source", "Choose Data Source:",
                         choices = c("Generate Synthetic Data" = "synthetic",
                                     "Upload Your Own Data" = "upload"),
                         selected = "synthetic",
                         inline = TRUE),

            hr(),

            # Synthetic Data Options
            conditionalPanel(
              condition = "input.data_source == 'synthetic'",
              h4("Synthetic Data Settings"),
              fluidRow(
                column(4,
                       selectInput("family", "Model Family:",
                                   choices = c("Gaussian (Linear)" = "gaussian",
                                               "Binomial (Logistic)" = "binomial"),
                                   selected = "gaussian")
                ),
                column(4,
                       numericInput("n_obs", "Observations:",
                                    value = 150, min = 50, max = 1000, step = 10)
                ),
                column(4,
                       numericInput("n_pred", "Predictors:",
                                    value = 8, min = 4, max = 30, step = 1)
                )
              ),
              actionButton("generate_data", "Generate Data",
                           class = "btn-success btn-lg",
                           icon = icon("dice"))
            ),

            # Upload Data Options
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              h4("Upload Your Data"),

              fluidRow(
                column(12,
                       fileInput("data_file", "Choose File (CSV, TXT, XLS, XLSX):",
                                 accept = c("text/csv",
                                            "text/comma-separated-values",
                                            "text/plain",
                                            ".csv",
                                            ".txt",
                                            ".xlsx",
                                            ".xls",
                                            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                            "application/vnd.ms-excel"),
                                 buttonLabel = "Browse...",
                                 placeholder = "No file selected")
                )
              ),

              uiOutput("file_info"),

              fluidRow(
                column(6,
                       conditionalPanel(
                         condition = "output.file_type == 'text'",
                         selectInput("separator", "Delimiter:",
                                     choices = c("Comma (,)" = ",",
                                                 "Semicolon (;)" = ";",
                                                 "Tab" = "\t",
                                                 "Space" = " "),
                                     selected = ",")
                       ),
                       conditionalPanel(
                         condition = "output.file_type == 'excel'",
                         uiOutput("sheet_selector")
                       )
                ),
                column(6,
                       checkboxInput("header", "First row contains headers", TRUE)
                )
              ),

              fluidRow(
                column(6,
                       selectInput("upload_family", "Model Family:",
                                   choices = c("Gaussian (Linear)" = "gaussian",
                                               "Binomial (Logistic)" = "binomial"),
                                   selected = "gaussian")
                ),
                column(6,
                       conditionalPanel(
                         condition = "input.data_file",
                         tags$div(
                           style = "margin-top: 25px;",
                           actionButton("refresh_data", "Reload Data",
                                        class = "btn-info btn-sm")
                         )
                       )
                )
              ),

              hr(),
              uiOutput("variable_selection")
            )
          )
        ),

        # Data Preview
        fluidRow(
          box(
            title = "Data Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,

            verbatimTextOutput("data_summary"),
            hr(),
            DTOutput("data_preview")
          )
        ),

        # ========== NEW: FEATURE ENGINEERING & TRAIN/TEST SPLIT ==========
        fluidRow(
          box(
            title = "Feature Engineering & Train/Test Split",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,

            fluidRow(
              column(6,
                     h4("Feature Engineering"),
                     checkboxInput("add_interactions",
                                   "Add Interaction Terms",
                                   value = FALSE),
                     helpText("Creates pairwise interaction terms between predictors")
              ),
              column(6,
                     h4("Train/Test Split"),
                     sliderInput("train_pct",
                                 "Training Set Percentage:",
                                 min = 50, max = 90, value = 70, step = 5),
                     helpText("Percentage of data to use for training")
              )
            ),

            actionButton("apply_features", "Apply Settings",
                         class = "btn-primary btn-lg",
                         icon = icon("cogs"))
          )
        )
      ),

      # ==================== Parameters Tab ====================
      tabItem(
        tabName = "parameters",
        fluidRow(
          box(
            title = "Algorithm Parameters",
            status = "primary",
            solidHeader = TRUE,
            width = 6,

            sliderInput("K", "Maximum Steps (K):",
                        min = 3, max = 20, value = 8, step = 1),

            sliderInput("delta", "AIC Branching Tolerance (δ):",
                        min = 0, max = 5, value = 2, step = 0.1),

            sliderInput("L", "Max Models per Step (L):",
                        min = 10, max = 200, value = 50, step = 10),

            helpText("• K: Maximum forward selection steps",
                     "• δ: Keeps models within this AIC range",
                     "• L: Limits computational growth")
          ),

          box(
            title = "Stability & Filtering",
            status = "success",
            solidHeader = TRUE,
            width = 6,

            sliderInput("B", "Bootstrap Resamples (B):",
                        min = 10, max = 200, value = 50, step = 10),

            sliderInput("Delta", "Plausibility Tolerance (Δ):",
                        min = 0, max = 5, value = 2, step = 0.1),

            sliderInput("tau", "Stability Threshold (τ):",
                        min = 0, max = 1, value = 0.6, step = 0.05),

            helpText("• B: Number of resamples for stability",
                     "• Δ: AIC tolerance for final models",
                     "• τ: Minimum average stability required")
          )
        ),

        fluidRow(
          box(
            title = "Run Analysis",
            status = "warning",
            solidHeader = TRUE,
            width = 12,

            actionButton("run", "Run Multi-Path AIC Analysis",
                         class = "btn-warning btn-lg btn-block",
                         icon = icon("rocket")),
            br(),
            uiOutput("run_status")
          )
        )
      ),

      # ==================== Results Tab ====================
      tabItem(
        tabName = "results",

        fluidRow(
          valueBoxOutput("steps_box", width = 3),
          valueBoxOutput("models_box", width = 3),
          valueBoxOutput("aic_box", width = 3),
          valueBoxOutput("time_box", width = 3)
        ),

        fluidRow(
          box(
            title = "Variable Stability Scores",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("stability_plot", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Models Retained by Step",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("models_by_step", height = "350px")
          ),

          box(
            title = "Top Variables Summary",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("top_variables")
          )
        )
      ),

      # ==================== Plausible Models Tab ====================
      tabItem(
        tabName = "plausible",

        fluidRow(
          box(
            title = "Selected Plausible Models",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("plausible_table")
          )
        ),

        fluidRow(
          box(
            title = "Model Overlap Heatmap",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("overlap_heatmap", height = "500px")
          ),

          box(
            title = "Variable Inclusion Probabilities",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("inclusion_plot", height = "500px")
          )
        )
      ),

      # ==================== Branching Tree Tab ====================
      tabItem(
        tabName = "branching",

        fluidRow(
          box(
            title = "Multi-Path Branching Visualization",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("branching_tree", height = "600px"),
            hr(),
            uiOutput("branching_info")
          )
        )
      ),


      # ==================== Enhanced Diagnostics Tab ====================
      tabItem(
        tabName = "diagnostics",

        # ========== NEW: TEST SET EVALUATION BOX ==========
        fluidRow(
          box(
            title = "Test Set Evaluation",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            conditionalPanel(
              condition = "output.has_test_data",
              uiOutput("test_eval_content")
            ),

            conditionalPanel(
              condition = "!output.has_test_data",
              tags$div(
                class = "alert alert-info",
                icon("info-circle"),
                " No test set available. Apply train/test split in the Data tab to enable test evaluation."
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "Best Model Performance",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            uiOutput("performance_cards")
          )
        ),

        # Binomial Diagnostics
        conditionalPanel(
          condition = "input.data_source == 'synthetic' ? input.family == 'binomial' : input.upload_family == 'binomial'",

          fluidRow(
            box(
              title = "Confusion Matrix",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              plotOutput("confusion_plot", height = "400px")
            ),

            box(
              title = "Performance Metrics",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              uiOutput("metrics_boxes")
            )
          ),

          fluidRow(
            box(
              title = "ROC Curve",
              status = "warning",
              solidHeader = TRUE,
              width = 6,
              plotOutput("roc_curve", height = "400px")
            ),

            box(
              title = "Prediction Distribution",
              status = "success",
              solidHeader = TRUE,
              width = 6,
              plotOutput("pred_distribution", height = "400px")
            )
          )
        ),

        # Gaussian Diagnostics
        conditionalPanel(
          condition = "input.data_source == 'synthetic' ? input.family == 'gaussian' : input.upload_family == 'gaussian'",

          fluidRow(
            box(
              title = "Residual Diagnostics",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              plotOutput("residual_plot", height = "400px")
            ),

            box(
              title = "Predicted vs Actual",
              status = "info",
              solidHeader = TRUE,
              width = 6,
              plotOutput("pred_actual_plot", height = "400px")
            )
          )
        ),

        fluidRow(
          box(
            title = "Download Report",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            downloadButton("download_report", "Download Full Report",
                           class = "btn-success btn-lg btn-block")
          )
        )
      ),

      # ==================== About Tab ====================
      tabItem(
        tabName = "about",

        fluidRow(
          box(
            title = "About Multi-Path AIC Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            h3("Multi-Path AIC Selection"),
            p("This Shiny app demonstrates the", code("multipathaic"),
              "R package for multi-path forward selection using AIC."),

            h4("Key Features"),
            tags$ul(
              tags$li("Flexible Data Input: Upload CSV, TXT, XLS, or XLSX files"),
              tags$li("Generate synthetic data for testing and demonstrations"),
              tags$li("Explores multiple competitive model paths simultaneously"),
              tags$li("Assesses variable stability via bootstrap resampling"),
              tags$li("Identifies plausible models balancing fit and stability"),
              tags$li("Supports both Gaussian and binomial regression"),
              tags$li("Comprehensive diagnostic visualizations")
            ),

            h4("Algorithms"),
            tags$ol(
              tags$li(strong("build_paths()"), " - Multi-path forward selection with branching"),
              tags$li(strong("stability()"), " - Bootstrap stability estimation"),
              tags$li(strong("plausible_models()"), " - AIC + stability filtering"),
              tags$li(strong("multipath_aic()"), " - Complete pipeline")
            ),

            h4("Installation"),
            pre('install.packages("remotes")\nremotes::install_github("R-4-Data-Science/FinalProjectmultipathaic")'),

            h4("Required Packages"),
            p("The app uses the following packages:"),
            tags$ul(
              tags$li(code("shiny"), " - Web application framework"),
              tags$li(code("shinydashboard"), " - Dashboard layout"),
              tags$li(code("plotly"), " - Interactive plots"),
              tags$li(code("DT"), " - Interactive tables"),
              tags$li(code("ggplot2"), " - Static plots"),
              tags$li(code("readxl"), " - Excel file support (auto-installed if needed)")
            ),
            p("Install all at once:"),
            pre('install.packages(c("shiny", "shinydashboard", "plotly", "DT", "ggplot2", "readxl"))'),

            h4("Authors"),
            p("Michael Obuobi, Jinchen Jiang, Farkhonda Rahmati"),
            p("Auburn University, 2025"),

            hr(),
            p("Repository:",
              a("GitHub",
                href = "https://github.com/R-4-Data-Science/FinalProjectmultipathaic",
                target = "_blank", class = "btn btn-primary"))
          )
        )
      )
    )
  )
)

# ==================== Server Logic ====================
server <- function(input, output, session) {

  convert_to_numeric <- function(X, y) {
    tryCatch({
      # Convert response to numeric if it's categorical
      if (is.character(y) || is.factor(y)) {
        y <- as.numeric(as.factor(y)) - 1  # Convert to 0/1 for binary
      }

      # Process each predictor column
      X_numeric <- as.data.frame(lapply(X, function(col) {
        if (is.numeric(col)) {
          return(col)
        } else if (is.character(col) || is.factor(col)) {
          # Check if binary (Yes/No, Male/Female, etc.)
          unique_vals <- unique(na.omit(col))
          if (length(unique_vals) == 2) {
            # Binary variable: convert to 0/1
            return(as.numeric(as.factor(col)) - 1)
          } else if (length(unique_vals) <= 5) {
            # Few categories: convert to numeric
            return(as.numeric(as.factor(col)) - 1)
          } else {
            # Many categories: skip this variable
            return(NULL)
          }
        } else {
          return(as.numeric(col))
        }
      }))

      # Remove NULL columns
      X_numeric <- X_numeric[, !sapply(X_numeric, is.null), drop = FALSE]

      # Ensure all columns are numeric
      X_numeric <- as.data.frame(lapply(X_numeric, as.numeric))

      return(list(X = X_numeric, y = as.numeric(y)))

    }, error = function(e) {
      stop(paste("Error converting data to numeric:", e$message))
    })
  }

  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    X = NULL,
    y = NULL,
    X_train = NULL,   # NEW
    X_test = NULL,    # NEW
    y_train = NULL,   # NEW
    y_test = NULL,    # NEW
    result = NULL,
    run_time = NULL,
    family = "gaussian"
  )

  # File type detection
  file_type <- reactiveVal("none")
  available_sheets <- reactiveVal(NULL)

  output$file_type <- reactive({
    file_type()
  })
  outputOptions(output, "file_type", suspendWhenHidden = FALSE)

  # Helper function to read files
  read_data_file <- function(file_path, file_ext, separator = ",",
                             header = TRUE, sheet = 1) {

    if (file_ext %in% c("xlsx", "xls")) {
      # Check if readxl is available
      if (!requireNamespace("readxl", quietly = TRUE)) {
        showNotification(
          "Installing 'readxl' package for Excel support...",
          type = "warning",
          duration = 5
        )
        install.packages("readxl")
      }

      library(readxl)
      df <- read_excel(file_path, sheet = sheet)
      df <- as.data.frame(df)

      # Clean column names
      names(df) <- make.names(names(df), unique = TRUE)

    } else if (file_ext %in% c("csv", "txt")) {
      df <- read.table(
        file_path,
        header = header,
        sep = separator,
        stringsAsFactors = FALSE,
        check.names = TRUE
      )
    } else {
      stop("Unsupported file format. Supported: CSV, TXT, XLS, XLSX")
    }

    return(df)
  }

  # ==================== Generate Synthetic Data ====================
  observeEvent(input$generate_data, {
    set.seed(123)
    n <- input$n_obs
    p <- input$n_pred
    X <- as.data.frame(matrix(rnorm(n*p), n, p))
    names(X) <- paste0("x", 1:p)

    if (input$family == "gaussian") {
      beta <- c(2, -1.5, 1, rep(0, max(0, p-3)))
      y <- as.numeric(as.matrix(X) %*% beta + rnorm(n, 1))
    } else {
      n_important <- min(3, p)
      coefs <- rep(0, p)
      if (n_important > 0) {
        coefs[1:n_important] <- c(1.5, -2, 1)[1:n_important]
      }
      eta <- as.numeric(as.matrix(X) %*% coefs)
      prob <- 1 / (1 + exp(-eta))
      y <- rbinom(n, 1, prob)
    }

    # Make sure everything is numeric
    rv$data <- data.frame(y = as.numeric(y), X)
    rv$X <- X
    rv$y <- as.numeric(y)
    rv$family <- input$family

    showNotification("Synthetic data generated!", type = "message", duration = 3)
  })


  # ==================== File Info Display ====================
  output$file_info <- renderUI({
    req(input$data_file)

    file_ext <- tools::file_ext(input$data_file$name)
    file_size <- round(file.size(input$data_file$datapath) / 1024, 2)

    tags$div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(" File Info: "),
      input$data_file$name, " | ",
      "Type: ", toupper(file_ext), " | ",
      "Size: ", file_size, " KB"
    )
  })

  # ==================== Sheet Selector for Excel ====================
  output$sheet_selector <- renderUI({
    req(input$data_file)
    req(available_sheets())

    selectInput("excel_sheet", "Select Sheet:",
                choices = available_sheets(),
                selected = available_sheets()[1])
  })

  # ==================== Upload and Read Data ====================
  observeEvent(input$data_file, {
    req(input$data_file)

    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tolower(tools::file_ext(input$data_file$name))

      if (file_ext %in% c("xlsx", "xls")) {
        # Excel file
        file_type("excel")

        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification(
            "Installing 'readxl' package...",
            type = "warning",
            duration = 5
          )
          install.packages("readxl")
        }

        library(readxl)

        # Get available sheets
        sheets <- excel_sheets(file_path)
        available_sheets(sheets)

        if (length(sheets) > 1) {
          showNotification(
            paste("Excel file has", length(sheets), "sheets. Please select one."),
            type = "message",
            duration = 5
          )
        }

        # Read first sheet by default
        df <- read_excel(file_path, sheet = 1)
        df <- as.data.frame(df)
        names(df) <- make.names(names(df), unique = TRUE)

      } else if (file_ext %in% c("csv", "txt")) {
        # Text file
        file_type("text")
        available_sheets(NULL)

        df <- read.table(
          file_path,
          header = input$header,
          sep = input$separator,
          stringsAsFactors = FALSE,
          check.names = TRUE
        )
      } else {
        stop("Unsupported file format. Please use CSV, TXT, XLS, or XLSX.")
      }

      rv$data <- df
      showNotification("Data uploaded successfully!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # ==================== Update Data When Excel Sheet Changes ====================
  observeEvent(input$excel_sheet, {
    req(input$data_file, input$excel_sheet)
    req(file_type() == "excel")

    tryCatch({
      library(readxl)

      df <- read_excel(
        input$data_file$datapath,
        sheet = input$excel_sheet
      )
      df <- as.data.frame(df)
      names(df) <- make.names(names(df), unique = TRUE)

      rv$data <- df
      showNotification(
        paste("Loaded sheet:", input$excel_sheet),
        type = "message",
        duration = 3
      )

    }, error = function(e) {
      showNotification(
        paste("Error loading sheet:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # ==================== Refresh Data Button ====================
  observeEvent(input$refresh_data, {
    req(input$data_file)

    showNotification("Refreshing data...", type = "message", duration = 2)

    file_path <- input$data_file$datapath
    file_ext <- tolower(tools::file_ext(input$data_file$name))

    tryCatch({
      if (file_ext %in% c("xlsx", "xls")) {
        library(readxl)
        sheet_to_use <- if (!is.null(input$excel_sheet)) input$excel_sheet else 1
        df <- read_excel(file_path, sheet = sheet_to_use)
        df <- as.data.frame(df)
        names(df) <- make.names(names(df), unique = TRUE)
      } else {
        df <- read.table(
          file_path,
          header = input$header,
          sep = input$separator,
          stringsAsFactors = FALSE,
          check.names = TRUE
        )
      }

      rv$data <- df
      showNotification("Data refreshed!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })

  # ==================== Variable Selection UI ====================
  output$variable_selection <- renderUI({
    req(rv$data)

    vars <- names(rv$data)

    tagList(
      h4("Select Variables"),
      selectInput("response_var", "Response Variable (y):",
                  choices = vars, selected = vars[1]),
      selectInput("predictor_vars", "Predictor Variables (X):",
                  choices = vars, selected = vars[-1], multiple = TRUE),
      actionButton("confirm_vars", "Confirm Selection",
                   class = "btn-success", icon = icon("check"))
    )
  })

  # ==================== Confirm Variable Selection ====================
  observeEvent(input$confirm_vars, {
    req(rv$data, input$response_var, input$predictor_vars)

    tryCatch({
      y_raw <- rv$data[[input$response_var]]
      X_raw <- rv$data[, input$predictor_vars, drop = FALSE]

      # Convert to numeric
      converted <- convert_to_numeric(X_raw, y_raw)

      rv$X <- converted$X
      rv$y <- converted$y
      rv$family <- input$upload_family

      # Notify user about conversions
      n_converted <- sum(sapply(X_raw, function(col) !is.numeric(col)))

      if (n_converted > 0) {
        showNotification(
          paste("Converted", n_converted, "categorical variable(s) to numeric (0/1 coding)"),
          type = "message",
          duration = 5
        )
      } else {
        showNotification("Variables selected!", type = "message", duration = 3)
      }

    }, error = function(e) {
      showNotification(
        paste("Error processing variables:", e$message),
        type = "error",
        duration = 10
      )
    })
  })


  # ==================== Apply Feature Engineering & Split ====================
  observeEvent(input$apply_features, {
    req(rv$X, rv$y)

    tryCatch({
      X_processed <- rv$X

      # Add interactions if requested
      if (input$add_interactions) {
        showNotification("Creating interaction terms...",
                         type = "message", duration = 3)

        # Create all pairwise interactions
        n_orig <- ncol(X_processed)
        interaction_data <- list()

        for (i in 1:(n_orig-1)) {
          for (j in (i+1):n_orig) {
            var_name <- paste0(colnames(X_processed)[i], ".",
                               colnames(X_processed)[j])
            interaction_data[[var_name]] <- X_processed[[i]] * X_processed[[j]]
          }
        }

        # Combine original and interactions
        X_processed <- cbind(X_processed, as.data.frame(interaction_data))

        showNotification(
          paste("Added", length(interaction_data), "interaction terms"),
          type = "message", duration = 5
        )
      }

      # Perform train/test split
      set.seed(123)
      n <- nrow(X_processed)
      train_size <- floor(n * input$train_pct / 100)
      train_idx <- sample(1:n, train_size)

      rv$X_train <- X_processed[train_idx, , drop = FALSE]
      rv$X_test <- X_processed[-train_idx, , drop = FALSE]
      rv$y_train <- rv$y[train_idx]
      rv$y_test <- rv$y[-train_idx]

      showNotification(
        paste0("Split complete: ", nrow(rv$X_train), " training, ",
               nrow(rv$X_test), " test samples"),
        type = "message", duration = 5
      )

    }, error = function(e) {
      showNotification(
        paste("Error:", e$message),
        type = "error", duration = 10
      )
    })
  })



  # ==================== Data Summary ====================
  output$data_summary <- renderPrint({
    req(rv$X, rv$y)

    cat("Data Summary\n")
    cat("================\n\n")
    cat("Observations:", nrow(rv$X), "\n")
    cat("Predictors:", ncol(rv$X), "\n")
    cat("Response type:", class(rv$y), "\n")
    cat("Response range:", paste(round(range(rv$y, na.rm = TRUE), 2), collapse = " to "), "\n")

    if (rv$family == "binomial") {
      cat("\nResponse distribution:\n")
      print(table(rv$y))
      cat("(Note: Categorical variables converted to 0/1)\n")
    }

    cat("\nPredictor summary:\n")
    print(summary(rv$X))

    cat("\n All variables are now numeric and ready for analysis\n")
  })


  # ==================== Data Preview ====================
  output$data_preview <- renderDT({
    req(rv$X, rv$y)

    preview_df <- data.frame(y = rv$y, rv$X)
    datatable(preview_df,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  # ==================== Run Analysis ====================
  observeEvent(input$run, {
    # Use split data if available, otherwise use all data
    X_to_use <- if (!is.null(rv$X_train)) rv$X_train else rv$X
    y_to_use <- if (!is.null(rv$y_train)) rv$y_train else rv$y

    req(X_to_use, y_to_use)

    withProgress(message = 'Running Multi-Path AIC...', value = 0, {

      incProgress(0.2, detail = "Building paths...")
      start_time <- Sys.time()

      result <- multipath_aic(
        X = X_to_use,
        y = y_to_use,
        family = rv$family,
        K = input$K,
        eps = 1e-6,
        delta = input$delta,
        L = input$L,
        B = input$B,
        resample_fraction = 0.8,
        Delta = input$Delta,
        tau = input$tau,
        verbose = FALSE
      )

      end_time <- Sys.time()
      rv$run_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      rv$result <- result

      incProgress(1, detail = "Complete!")
    })

    showNotification("Analysis complete!", type = "message", duration = 5)
  })


  # ==================== Run Status ====================
  output$run_status <- renderUI({
    if (!is.null(rv$result)) {
      tagList(
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          strong(" Success! "),
          "Analysis completed in ", round(rv$run_time, 1), " seconds."
        )
      )
    }
  })

  # ==================== Value Boxes ====================
  output$steps_box <- renderValueBox({
    req(rv$result)
    n_steps <- length(rv$result$forest$path_forest$frontiers)
    valueBox(
      n_steps,
      "Steps Explored",
      icon = icon("list-ol"),
      color = "blue"
    )
  })

  output$models_box <- renderValueBox({
    req(rv$result)
    n_models <- nrow(rv$result$plaus$plausible_models)
    valueBox(
      n_models,
      "Plausible Models",
      icon = icon("layer-group"),
      color = "green"
    )
  })

  output$aic_box <- renderValueBox({
    req(rv$result)
    best_aic <- round(rv$result$plaus$best_aic, 2)
    valueBox(
      best_aic,
      "Best AIC",
      icon = icon("trophy"),
      color = "yellow"
    )
  })

  output$time_box <- renderValueBox({
    req(rv$result)
    valueBox(
      paste0(round(rv$run_time, 1), "s"),
      "Run Time",
      icon = icon("clock"),
      color = "red"
    )
  })

  # ==================== Stability Plot ====================
  output$stability_plot <- renderPlotly({
    req(rv$result)

    pi <- rv$result$stab$pi
    df <- data.frame(
      Variable = names(pi),
      Stability = as.numeric(pi)
    )
    df <- df[order(df$Stability, decreasing = TRUE), ]
    df$Variable <- factor(df$Variable, levels = df$Variable)

    p <- ggplot(df, aes(x = Variable, y = Stability)) +
      geom_col(aes(fill = Stability)) +
      geom_hline(yintercept = input$tau, linetype = "dashed",
                 color = "red", size = 1) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = NULL,
           x = "Variable", y = "Stability Score (π)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10))

    ggplotly(p, tooltip = c("x", "y"))
  })

  # ==================== Models by Step ====================
  output$models_by_step <- renderPlot({
    req(rv$result)

    frontiers <- rv$result$forest$path_forest$frontiers
    n_models <- sapply(frontiers, nrow)

    df <- data.frame(
      Step = 1:length(n_models),
      Models = n_models
    )

    ggplot(df, aes(x = Step, y = Models)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_point(color = "steelblue", size = 4) +
      geom_area(alpha = 0.3, fill = "steelblue") +
      labs(title = NULL,
           x = "Forward Selection Step",
           y = "Number of Models") +
      theme_minimal() +
      theme(text = element_text(size = 12))
  })

  # ==================== Top Variables ====================
  output$top_variables <- renderDT({
    req(rv$result)

    pi <- rv$result$stab$pi
    top_vars <- head(sort(pi, decreasing = TRUE), 10)

    df <- data.frame(
      Rank = 1:length(top_vars),
      Variable = names(top_vars),
      Stability = round(as.numeric(top_vars), 3)
    )

    datatable(df,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE) %>%
      formatStyle('Stability',
                  background = styleColorBar(df$Stability, 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })

  # ==================== Plausible Models Table ====================
  output$plausible_table <- renderDT({
    req(rv$result)

    plaus <- rv$result$plaus$plausible_models

    display_df <- data.frame(
      Model_ID = seq_len(nrow(plaus)),
      AIC = round(plaus$AIC, 2),
      Variables = sapply(plaus$model, function(x) paste(x, collapse = ", ")),
      Avg_Stability = round(plaus$avg_stability, 3),
      N_Vars = sapply(plaus$model, length)
    )

    datatable(display_df,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('Avg_Stability',
                  background = styleColorBar(range(display_df$Avg_Stability), 'lightgreen'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })

  # ==================== Overlap Heatmap ====================
  output$overlap_heatmap <- renderPlotly({
    req(rv$result)

    plaus <- rv$result$plaus$plausible_models

    if (nrow(plaus) < 2) {
      return(plotly_empty() %>%
               layout(title = "Need at least 2 plausible models"))
    }

    n_models <- min(nrow(plaus), 15)
    plaus_subset <- plaus[1:n_models, ]

    overlap_matrix <- matrix(0, n_models, n_models)

    for (i in 1:n_models) {
      for (j in 1:n_models) {
        vars_i <- plaus_subset$model[[i]]
        vars_j <- plaus_subset$model[[j]]

        if (length(vars_i) == 0 || length(vars_j) == 0) {
          overlap_matrix[i, j] <- 0
        } else {
          overlap <- length(intersect(vars_i, vars_j)) /
            length(union(vars_i, vars_j))
          overlap_matrix[i, j] <- overlap
        }
      }
    }

    model_labels <- paste("M", 1:n_models)

    plot_ly(
      x = model_labels,
      y = model_labels,
      z = overlap_matrix,
      type = "heatmap",
      colors = colorRamp(c("white", "orange", "red")),
      hovertemplate = '%{x} & %{y}<br>Jaccard: %{z:.2f}<extra></extra>'
    ) %>%
      layout(
        title = "Jaccard Overlap Between Models",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })

  # ==================== Inclusion Plot ====================
  output$inclusion_plot <- renderPlotly({
    req(rv$result)

    inclusion <- rv$result$plaus$inclusion
    df <- data.frame(
      Variable = names(inclusion),
      Inclusion = as.numeric(inclusion)
    )
    df <- df[order(df$Inclusion, decreasing = TRUE), ]
    df$Variable <- factor(df$Variable, levels = df$Variable)

    p <- ggplot(df, aes(x = Variable, y = Inclusion)) +
      geom_col(aes(fill = Inclusion)) +
      geom_hline(yintercept = 1, linetype = "dashed",
                 color = "darkgreen", size = 1) +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
      labs(title = NULL,
           x = "Variable", y = "Inclusion Probability") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = c("x", "y"))
  })

  # ==================== Branching Tree ====================
  output$branching_tree <- renderPlotly({
    req(rv$result)

    frontiers <- rv$result$forest$path_forest$frontiers

    all_models_list <- list()
    for (step in seq_along(frontiers)) {
      frontier <- frontiers[[step]]
      for (i in 1:min(nrow(frontier), 30)) {
        all_models_list[[length(all_models_list) + 1]] <- list(
          step = step,
          aic = frontier$AIC[i],
          vars = paste(frontier$model[[i]], collapse = ", "),
          n_vars = length(frontier$model[[i]])
        )
      }
    }

    nodes_df <- do.call(rbind, lapply(all_models_list, function(m) {
      data.frame(
        step = m$step,
        aic = m$aic,
        vars = m$vars,
        n_vars = m$n_vars,
        stringsAsFactors = FALSE
      )
    }))

    plot_ly(nodes_df) %>%
      add_trace(
        type = "scatter",
        x = ~step,
        y = ~aic,
        text = ~paste("Step:", step, "<br>AIC:", round(aic, 2),
                      "<br>Vars:", vars),
        mode = "markers",
        marker = list(
          size = ~n_vars * 3 + 5,
          color = ~step,
          colorscale = "Viridis",
          showscale = TRUE,
          colorbar = list(title = "Step"),
          line = list(color = "white", width = 1)
        ),
        hovertemplate = '%{text}<extra></extra>'
      ) %>%
      layout(
        title = "Multi-Path Exploration (size = # variables)",
        xaxis = list(title = "Forward Selection Step"),
        yaxis = list(title = "AIC")
      )
  })

  # ==================== Branching Info ====================
  output$branching_info <- renderUI({
    req(rv$result)

    frontiers <- rv$result$forest$path_forest$frontiers
    total_models <- sum(sapply(frontiers, nrow))

    tags$div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(" Info: "),
      "Total unique models explored: ", total_models, " | ",
      "Final step: ", length(frontiers), " | ",
      "Point size indicates number of variables"
    )
  })

  # ==================== Performance Cards ====================
  output$performance_cards <- renderUI({
    req(rv$result)

    plaus <- rv$result$plaus$plausible_models

    if (nrow(plaus) == 0) {
      return(tags$div(class = "alert alert-warning", "No plausible models found."))
    }

    best_vars <- plaus$model[[1]]
    best_aic <- plaus$AIC[1]
    best_stability <- plaus$avg_stability[1]

    fluidRow(
      valueBox(
        length(best_vars),
        "Variables Selected",
        icon = icon("list"),
        color = "blue",
        width = 3
      ),
      valueBox(
        round(best_aic, 2),
        "Best AIC",
        icon = icon("trophy"),
        color = "yellow",
        width = 3
      ),
      valueBox(
        round(best_stability, 3),
        "Avg Stability",
        icon = icon("check-circle"),
        color = "green",
        width = 3
      ),
      valueBox(
        nrow(plaus),
        "Plausible Models",
        icon = icon("layer-group"),
        color = "purple",
        width = 3
      )
    )
  })

  # ==================== Confusion Plot (FIXED) ====================
  output$confusion_plot <- renderPlot({
    req(rv$result)
    req(rv$family == "binomial")

    plaus_set <- rv$result$plaus$plausible_models
    if (nrow(plaus_set) == 0) return(NULL)

    tryCatch({
      selected_vars <- plaus_set$model[[1]]
      X <- rv$result$forest$model_data$X
      y <- rv$result$forest$model_data$y

      # Create data frame and remove NA values
      df <- data.frame(y = y, X)
      df_clean <- na.omit(df)

      if (nrow(df_clean) < 10) {
        plot.new()
        text(0.5, 0.5, "Insufficient data after removing missing values", cex = 1.2)
        return(NULL)
      }

      form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
      model <- glm(form, data = df_clean, family = binomial())

      p_hat <- predict(model, type = "response")
      y_pred <- ifelse(p_hat >= 0.5, 1, 0)
      y_actual <- df_clean$y

      TP <- sum(y_pred == 1 & y_actual == 1)
      TN <- sum(y_pred == 0 & y_actual == 0)
      FP <- sum(y_pred == 1 & y_actual == 0)
      FN <- sum(y_pred == 0 & y_actual == 1)

      conf_df <- data.frame(
        Predicted = rep(c("Positive", "Negative"), each = 2),
        Actual = rep(c("Positive", "Negative"), 2),
        Count = c(TP, FN, FP, TN),
        Label = c("TP", "FN", "FP", "TN")
      )

      ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
        geom_tile(color = "white", size = 2) +
        geom_text(aes(label = paste0(Label, "\n", Count)),
                  size = 12, color = "white", fontface = "bold") +
        scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
        labs(title = "Confusion Matrix (Cutoff = 0.5)",
             x = "Actual Class", y = "Predicted Class",
             caption = paste("n =", nrow(df_clean), "observations")) +
        theme_minimal() +
        theme(text = element_text(size = 14),
              axis.text = element_text(size = 12, face = "bold"),
              legend.position = "right")

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1.2)
    })
  })

  # ==================== Metrics Boxes (FIXED) ====================
  output$metrics_boxes <- renderUI({
    req(rv$result)
    req(rv$family == "binomial")

    plaus_set <- rv$result$plaus$plausible_models
    if (nrow(plaus_set) == 0) return(NULL)

    tryCatch({
      selected_vars <- plaus_set$model[[1]]
      X <- rv$result$forest$model_data$X
      y <- rv$result$forest$model_data$y

      # Create data frame and remove NA values
      df <- data.frame(y = y, X)
      df_clean <- na.omit(df)

      form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
      model <- glm(form, data = df_clean, family = binomial())

      p_hat <- predict(model, type = "response")
      y_pred <- ifelse(p_hat >= 0.5, 1, 0)
      y_actual <- df_clean$y

      TP <- sum(y_pred == 1 & y_actual == 1)
      TN <- sum(y_pred == 0 & y_actual == 0)
      FP <- sum(y_pred == 1 & y_actual == 0)
      FN <- sum(y_pred == 0 & y_actual == 1)

      accuracy <- (TP + TN) / (TP + TN + FP + FN)
      sensitivity <- TP / (TP + FN + 1e-8)
      specificity <- TN / (TN + FP + 1e-8)
      precision <- TP / (TP + FP + 1e-8)

      tagList(
        infoBox(
          "Accuracy",
          paste0(round(accuracy * 100, 1), "%"),
          icon = icon("check-circle"),
          color = "green",
          fill = TRUE,
          width = 6
        ),
        infoBox(
          "Sensitivity",
          paste0(round(sensitivity * 100, 1), "%"),
          icon = icon("heartbeat"),
          color = "blue",
          fill = TRUE,
          width = 6
        ),
        infoBox(
          "Specificity",
          paste0(round(specificity * 100, 1), "%"),
          icon = icon("shield-alt"),
          color = "yellow",
          fill = TRUE,
          width = 6
        ),
        infoBox(
          "Precision",
          paste0(round(precision * 100, 1), "%"),
          icon = icon("bullseye"),
          color = "purple",
          fill = TRUE,
          width = 6
        )
      )

    }, error = function(e) {
      tagList(
        tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " Unable to calculate metrics: ", e$message
        )
      )
    })
  })

  # ==================== ROC Curve (FIXED) ====================
  output$roc_curve <- renderPlot({
    req(rv$result)
    req(rv$family == "binomial")

    plaus_set <- rv$result$plaus$plausible_models
    if (nrow(plaus_set) == 0) return(NULL)

    tryCatch({
      selected_vars <- plaus_set$model[[1]]
      X <- rv$result$forest$model_data$X
      y <- rv$result$forest$model_data$y

      # Create data frame and remove NA values
      df <- data.frame(y = y, X)
      df_clean <- na.omit(df)

      form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
      model <- glm(form, data = df_clean, family = binomial())

      p_hat <- predict(model, type = "response")
      y_actual <- df_clean$y

      thresholds <- seq(0, 1, by = 0.01)
      roc_data <- lapply(thresholds, function(t) {
        y_pred <- ifelse(p_hat >= t, 1, 0)
        TP <- sum(y_pred == 1 & y_actual == 1)
        TN <- sum(y_pred == 0 & y_actual == 0)
        FP <- sum(y_pred == 1 & y_actual == 0)
        FN <- sum(y_pred == 0 & y_actual == 1)

        tpr <- TP / (TP + FN + 1e-8)
        fpr <- FP / (FP + TN + 1e-8)

        data.frame(FPR = fpr, TPR = tpr)
      })
      roc_df <- do.call(rbind, roc_data)

      auc <- abs(sum(diff(roc_df$FPR) * (head(roc_df$TPR, -1) + tail(roc_df$TPR, -1)) / 2))

      ggplot(roc_df, aes(x = FPR, y = TPR)) +
        geom_line(color = "#e74c3c", size = 2) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
        annotate("text", x = 0.7, y = 0.3,
                 label = paste("AUC =", round(auc, 3)),
                 size = 6, fontface = "bold") +
        labs(title = "ROC Curve",
             x = "False Positive Rate",
             y = "True Positive Rate",
             caption = paste("n =", nrow(df_clean), "observations")) +
        theme_minimal() +
        theme(text = element_text(size = 14))

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1.2)
    })
  })

  # ==================== Prediction Distribution (FIXED) ====================
  output$pred_distribution <- renderPlot({
    req(rv$result)
    req(rv$family == "binomial")

    plaus_set <- rv$result$plaus$plausible_models
    if (nrow(plaus_set) == 0) return(NULL)

    tryCatch({
      selected_vars <- plaus_set$model[[1]]
      X <- rv$result$forest$model_data$X
      y <- rv$result$forest$model_data$y

      # Create data frame and remove NA values
      df <- data.frame(y = y, X)
      df_clean <- na.omit(df)

      form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
      model <- glm(form, data = df_clean, family = binomial())

      p_hat <- predict(model, type = "response")
      y_actual <- df_clean$y

      pred_df <- data.frame(
        Probability = p_hat,
        Actual = factor(y_actual, levels = c(0, 1), labels = c("Negative", "Positive"))
      )

      ggplot(pred_df, aes(x = Probability, fill = Actual)) +
        geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
        geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
        scale_fill_manual(values = c("#3498db", "#e74c3c")) +
        labs(title = "Predicted Probability Distribution",
             x = "Predicted Probability",
             y = "Count",
             caption = paste("n =", nrow(df_clean), "observations")) +
        theme_minimal() +
        theme(text = element_text(size = 14),
              legend.position = "top")

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1.2)
    })
  })

  # ==================== Residual Plot (FIXED) ====================
  output$residual_plot <- renderPlot({
    req(rv$result)
    req(rv$family == "gaussian")

    plaus_set <- rv$result$plaus$plausible_models
    if (nrow(plaus_set) == 0) return(NULL)

    tryCatch({
      selected_vars <- plaus_set$model[[1]]
      X <- rv$result$forest$model_data$X
      y <- rv$result$forest$model_data$y

      # Create data frame and remove NA values
      df <- data.frame(y = y, X)
      df <- na.omit(df)  # Remove rows with missing values

      form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
      model <- lm(form, data = df)

      residuals_vals <- residuals(model)
      fitted_vals <- fitted(model)

      resid_df <- data.frame(
        Fitted = fitted_vals,
        Residuals = residuals_vals
      )

      ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
        geom_point(alpha = 0.6, color = "#3498db", size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
        geom_smooth(method = "loess", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
        labs(title = "Residual Plot",
             x = "Fitted Values",
             y = "Residuals") +
        theme_minimal() +
        theme(text = element_text(size = 14))

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1.2)
    })
  })

  # ==================== Predicted vs Actual (FIXED) ====================
  output$pred_actual_plot <- renderPlot({
    req(rv$result)
    req(rv$family == "gaussian")

    plaus_set <- rv$result$plaus$plausible_models
    if (nrow(plaus_set) == 0) return(NULL)

    tryCatch({
      selected_vars <- plaus_set$model[[1]]
      X <- rv$result$forest$model_data$X
      y <- rv$result$forest$model_data$y

      # Create data frame and remove NA values
      df <- data.frame(y = y, X)
      df_clean <- na.omit(df)  # Remove rows with missing values

      # Check if we have enough data
      if (nrow(df_clean) < 10) {
        plot.new()
        text(0.5, 0.5, "Insufficient data after removing missing values", cex = 1.2)
        return(NULL)
      }

      form <- as.formula(paste("y ~", paste(selected_vars, collapse = " + ")))
      model <- lm(form, data = df_clean)

      # Now both y and predictions have the same length
      pred_df <- data.frame(
        Actual = df_clean$y,
        Predicted = fitted(model)
      )

      r_squared <- summary(model)$r.squared
      rmse <- sqrt(mean((pred_df$Actual - pred_df$Predicted)^2))

      # Calculate plot ranges
      x_range <- range(pred_df$Actual)
      y_range <- range(pred_df$Predicted)

      ggplot(pred_df, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.6, color = "#3498db", size = 3) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                    color = "red", size = 1) +
        geom_smooth(method = "lm", se = TRUE, color = "#2ecc71",
                    fill = "#2ecc71", alpha = 0.2) +
        annotate("text",
                 x = x_range[1] + (x_range[2] - x_range[1]) * 0.05,
                 y = y_range[2] - (y_range[2] - y_range[1]) * 0.05,
                 label = paste0("R² = ", round(r_squared, 3), "\nRMSE = ", round(rmse, 3)),
                 size = 5, fontface = "bold", hjust = 0, vjust = 1) +
        labs(title = "Predicted vs Actual",
             x = "Actual Values",
             y = "Predicted Values",
             caption = paste("n =", nrow(pred_df), "observations")) +
        theme_minimal() +
        theme(text = element_text(size = 14))

    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating plot:\n", e$message), cex = 1.2)
    })
  })

  # ==================== Download Report ====================
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("multipathaic_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(rv$result)

      html_content <- paste0(
        "<!DOCTYPE html><html><head>",
        "<title>Multi-Path AIC Report</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; background-color: #f5f5f5; }",
        "h1 { color: #3c8dbc; }",
        "h2 { color: #00a65a; border-bottom: 2px solid #00a65a; padding-bottom: 10px; }",
        "table { border-collapse: collapse; width: 100%; margin: 20px 0; background: white; }",
        "th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }",
        "th { background-color: #3c8dbc; color: white; }",
        ".metric { background: white; padding: 20px; margin: 10px 0; border-radius: 5px; }",
        "</style>",
        "</head><body>",
        "<h1>Multi-Path AIC Selection Report</h1>",
        "<p><strong>Generated:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",

        "<h2>Data Summary</h2>",
        "<div class='metric'>",
        "<ul>",
        "<li><strong>Observations:</strong> ", nrow(rv$X), "</li>",
        "<li><strong>Predictors:</strong> ", ncol(rv$X), "</li>",
        "<li><strong>Family:</strong> ", rv$family, "</li>",
        "</ul>",
        "</div>",

        "<h2>Parameters Used</h2>",
        "<div class='metric'>",
        "<ul>",
        "<li><strong>K (Max Steps):</strong> ", input$K, "</li>",
        "<li><strong>δ (AIC Tolerance):</strong> ", input$delta, "</li>",
        "<li><strong>L (Max Models/Step):</strong> ", input$L, "</li>",
        "<li><strong>B (Resamples):</strong> ", input$B, "</li>",
        "<li><strong>Δ (Plausibility):</strong> ", input$Delta, "</li>",
        "<li><strong>τ (Stability):</strong> ", input$tau, "</li>",
        "</ul>",
        "</div>",

        "<h2>Results Summary</h2>",
        "<div class='metric'>",
        "<ul>",
        "<li><strong>Steps Explored:</strong> ", length(rv$result$forest$path_forest$frontiers), "</li>",
        "<li><strong>Plausible Models:</strong> ", nrow(rv$result$plaus$plausible_models), "</li>",
        "<li><strong>Best AIC:</strong> ", round(rv$result$plaus$best_aic, 2), "</li>",
        "<li><strong>Run Time:</strong> ", round(rv$run_time, 1), " seconds</li>",
        "</ul>",
        "</div>",

        "<h2>Plausible Models</h2>",
        "<table>",
        "<tr><th>Rank</th><th>AIC</th><th>Variables</th><th>Avg Stability</th><th># Vars</th></tr>"
      )

      plaus <- rv$result$plaus$plausible_models
      for (i in 1:min(nrow(plaus), 10)) {
        html_content <- paste0(
          html_content,
          "<tr>",
          "<td>", i, "</td>",
          "<td>", round(plaus$AIC[i], 2), "</td>",
          "<td>", paste(plaus$model[[i]], collapse = ", "), "</td>",
          "<td>", round(plaus$avg_stability[i], 3), "</td>",
          "<td>", length(plaus$model[[i]]), "</td>",
          "</tr>"
        )
      }

      html_content <- paste0(
        html_content,
        "</table>",

        "<h2>Top Stable Variables</h2>",
        "<table>",
        "<tr><th>Rank</th><th>Variable</th><th>Stability</th></tr>"
      )

      top_vars <- head(sort(rv$result$stab$pi, decreasing = TRUE), 10)
      for (i in 1:length(top_vars)) {
        html_content <- paste0(
          html_content,
          "<tr>",
          "<td>", i, "</td>",
          "<td>", names(top_vars)[i], "</td>",
          "<td>", round(top_vars[i], 3), "</td>",
          "</tr>"
        )
      }

      html_content <- paste0(
        html_content,
        "</table>",

        "<hr>",
        "<p><em>Report generated by multipathaic R package</em></p>",
        "<p><em>GitHub: <a href='https://github.com/R-4-Data-Science/FinalProjectmultipathaic'>",
        "github.com/R-4-Data-Science/FinalProjectmultipathaic</a></em></p>",
        "</body></html>"
      )

      writeLines(html_content, file)
    }
  )



  # ==================== Check if Test Data Exists ====================
  output$has_test_data <- reactive({
    !is.null(rv$X_test) && !is.null(rv$y_test) && !is.null(rv$result)
  })
  outputOptions(output, "has_test_data", suspendWhenHidden = FALSE)

  # ==================== Test Set Evaluation Content ====================
  output$test_eval_content <- renderUI({
    req(rv$result, rv$X_test, rv$y_test)

    plaus <- rv$result$plaus$plausible_models
    if (nrow(plaus) == 0) {
      return(tags$div(class = "alert alert-warning",
                      "No plausible models to evaluate."))
    }

    # Get best model
    best_vars <- plaus$model[[1]]

    # Use the training data we already have stored!
    X_train <- if (!is.null(rv$X_train)) rv$X_train else rv$X
    y_train <- if (!is.null(rv$y_train)) rv$y_train else rv$y

    train_df <- data.frame(y = y_train, X_train[, best_vars, drop = FALSE])

    if (rv$family == "gaussian") {
      model <- lm(y ~ ., data = train_df)

      # Predict on test
      test_df <- rv$X_test[, best_vars, drop = FALSE]
      y_pred <- predict(model, newdata = test_df)

      # Metrics
      test_rmse <- sqrt(mean((rv$y_test - y_pred)^2))
      train_pred <- predict(model)
      train_rmse <- sqrt(mean((y_train - train_pred)^2))
      test_cor <- cor(rv$y_test, y_pred)

      # Create table
      metrics_df <- data.frame(
        Metric = c("Variables Selected", "Training RMSE", "Test RMSE",
                   "Test Correlation", "Train R²", "Test R²"),
        Value = c(length(best_vars),
                  round(train_rmse, 3),
                  round(test_rmse, 3),
                  round(test_cor, 3),
                  round(summary(model)$r.squared, 3),
                  round(test_cor^2, 3))
      )

    } else {  # binomial
      model <- glm(y ~ ., data = train_df, family = binomial())

      # Predict on test
      test_df <- rv$X_test[, best_vars, drop = FALSE]
      y_pred_prob <- predict(model, newdata = test_df, type = "response")
      y_pred <- ifelse(y_pred_prob >= 0.5, 1, 0)

      # Metrics
      TP <- sum(y_pred == 1 & rv$y_test == 1)
      TN <- sum(y_pred == 0 & rv$y_test == 0)
      FP <- sum(y_pred == 1 & rv$y_test == 0)
      FN <- sum(y_pred == 0 & rv$y_test == 1)

      accuracy <- (TP + TN) / (TP + TN + FP + FN)
      sensitivity <- TP / (TP + FN + 1e-8)
      specificity <- TN / (TN + FP + 1e-8)

      metrics_df <- data.frame(
        Metric = c("Variables Selected", "Test Accuracy", "Test Sensitivity",
                   "Test Specificity", "TP", "TN", "FP", "FN"),
        Value = c(length(best_vars),
                  paste0(round(accuracy * 100, 1), "%"),
                  paste0(round(sensitivity * 100, 1), "%"),
                  paste0(round(specificity * 100, 1), "%"),
                  TP, TN, FP, FN)
      )
    }

    # Render table
    tagList(
      h4("Best Model Performance on Test Set"),
      tags$p(strong("Selected Variables: "),
             paste(best_vars, collapse = ", ")),
      hr(),
      renderTable(metrics_df, striped = TRUE, hover = TRUE, bordered = TRUE)
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
