#' Run the Inter-Rater Reliability shiny app.
#'
#' @export
IRRsim_demo <- function() {
	dir <- paste0(find.package('IRRsim'), '/shiny')
	message(paste0("Running shiny app from ", dir))
	shiny::runApp(appDir = dir)
}
