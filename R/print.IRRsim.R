#' Prints the results from simulateIRR.
#'
#' @param x the results of \code{\link{simulateIRR}}.
#' @param ... currently unused.
#' @export
print.IRRsim <- function(x, ...) {
	print(as.data.frame(x))
}
