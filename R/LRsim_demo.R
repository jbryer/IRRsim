#' Run the logistic regression shiny app.
#'
#' @export
LRsim_demo <- function() {
	dir <- paste0(find.package('IRRsim'), '/lr_shiny')
	message(paste0("Running shiny app from ", dir))
	shiny::runApp(appDir = dir)
}
