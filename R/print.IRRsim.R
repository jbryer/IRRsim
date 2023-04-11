#' Prints the results from `simulateIRR.`
#'
#' @param x the results of IRRsim::simulateIRR().
#' @param ... currently unused.
#' @return nothing returned.
#' @export
print.IRRsim <- function(x, ...) {
	print(as.data.frame(x))
}
