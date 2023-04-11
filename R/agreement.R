#' Calculate percent agreement.
#'
#' @param thedata rating matrix.
#' @return percent rater agreement.
#' @export
agreement <- function(thedata) {
	return(unname(prop.table(table(
		apply(thedata, 1, FUN=function(x) {
			length(unique(x[!is.na(x)])) == 1
		})))['TRUE']))
}
