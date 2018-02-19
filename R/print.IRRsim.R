#' Prints the results from simulateICC.
#'
#' @param x the results of \code{\link{simulateICC}}.
#' @param ... currently unused.
#' @export
print.IRRsim <- function(x, ...) {
	print(as.data.frame(x))
}
