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
  app_dir <- system.file("RS_int", package = "multipathaic")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing `multipathaic`.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
