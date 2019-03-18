#' Run the Inter-Rater Reliability shiny app.
#'
#' @export
IRRsim_demo <- function(clean = FALSE) {
	dir <- paste0(find.package('IRRsim'), '/shiny')
	message(paste0("Running shiny app from ", dir))
	cache.file <- paste0(dir, '/IRRsimShinyCache.rda')
	if(file.exists(cache.file)) {
		if(clean) {
			message(paste0('Deleting old cache file.'))
			unlink(paste0(dir, '/IRRsimShinyCache.rda'))
		} else {
			message('Cache file found. Set clean = TRUE to remove.')
		}
	}
	shiny::runApp(appDir = dir)
}
