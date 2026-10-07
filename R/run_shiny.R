#' Launch Interactive Shiny App
#'
#' @description
#' Launches an interactive Shiny application to explore the multi-path AIC
#' selection procedure with visualizations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' launch_app()
#' }
launch_app <- function() {
  needed <- c("shiny", "shinydashboard", "plotly", "DT", "ggplot2", "htmltools")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install optional app dependencies first: ",
                            paste(missing, collapse = ", "), call. = FALSE)
  app_dir <- system.file("RS_int", package = "multipathaic")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing `multipathaic`.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

