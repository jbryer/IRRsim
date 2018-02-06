#' Simulate a rating matrix.
#'
#' This function will generate a \code{nEvents} x \code{k} scoring matrix.
#'
#' @param nLevels the number of possible outcomes there are for each rating.
#' @param k the number of raters.
#' @param agree the percent of time the raters agree. Note that the actual
#'        agreement of the simulated matrix will vary from this value
#'        (see \link{sample}).
#' @param nEvents the number of rating events within each matrix.
#' @param response.probs probability weights for the distribution of scores. By default, each
#'        of the levels has equal probability of being selected. This allows situations where
#'        some responses are more common than others (e.g. 50\% of students get a 3, 30\% get a 2,
#'        and 20\% get a 1). This is independent of the percent agreement parameter.
#' @export
simulateRatingMatrix <- function(nLevels, k, agree, nEvents = 100,
								 response.probs = rep(1 / nLevels, nLevels)) {
	raters <- letters[1:k]
	thedata <- matrix(NA, nEvents * k, nrow = nEvents, ncol = k)
	dimnames(thedata) <- list(1:nEvents, raters)
	for(i in 1:nrow(thedata)) {
		row.raters <- sample(raters, 2)
		l <- base::sample(nLevels, prob = response.probs)
		# The likelihood of selecting another level is equally divided across the options.
		probs <- c(agree, rep((1 - agree) / (nLevels - 1), times = (nLevels - 1)))
		thedata[i,row.raters] <- c(l[1], base::sample(x = l, size = 1, prob = probs))
	}
	return(thedata)
}
