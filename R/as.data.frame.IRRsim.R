#' Converts the results of \code{\link{simulateICC}} to a \code{data.frame}
#'
#' This function will create a data frame with all the IRR metrics combiend.
#'
#' @param x the result of \code{\link{simulateICC}}.
#' @param ... currently unused.
#' @export
as.data.frame.IRRsim <- function(x, ...) {
	tmp <- sapply(x, FUN = function(y) {
		unlist(y[c('nLevels', 'nEvents', 'k', 'k_per_event', 'simAgreement', 'agreement',
				   'skewness', 'kurtosis', 'MaxResponseDiff',
				   'ICC1', 'ICC2', 'ICC3', 'ICC1k', 'ICC2k', 'ICC3k',
				   'Fleiss_Kappa', 'Cohen_Kappa')])
	}, simplify = "matrix")
	tmp <- as.data.frame(t(tmp))
	return(tmp)
}
