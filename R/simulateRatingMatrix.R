#' Simulate a rating matrix.
#'
#' This function will generate a \code{nEvents} x \code{k} scoring matrix.
#'
#' @param nLevels the number of possible outcomes there are for each rating.
#' @param k the total number of available raters.
#' @param k_per_event number of raters per scoring event.
#' @param agree the percent of time the raters agree. Note that the actual
#'        agreement of the simulated matrix will vary from this value
#'        (see \link{sample}).
#' @param nEvents the number of rating events within each matrix.
#' @param response.probs probability weights for the distribution of scores. By
#'        default, each of the levels has equal probability of being selected.
#'        This allows situations where some responses are more common than
#'        others (e.g. 50\% of students get a 3, 30\% get a 2, and 20\% get a 1).
#'        This is independent of the percent agreement parameter.
#' @export
#' @examples
#' test <- simulateRatingMatrix(nLevels = 3, k = 2, agree = 0.6, nEvents = 100)
#' psych::ICC(test)
simulateRatingMatrix <- function(nLevels, k, k_per_event = 2, agree, nEvents = 100,
								 response.probs = rep(1 / nLevels, nLevels)) {
	if(k_per_event < 2 | k_per_event > k) {
		stop(paste0('k_per_event must be between 2 and ', k))
	}
	if(agree < 0 | agree > 1) {
		stop('The agree parameter must be between 0 and 1.')
	}
	if(k > 26^2) {
		stop(paste0('Maximum number of raters is ', 26^2))
	}

	raters <- paste0(rep(letters[seq(1, ceiling(k / 26))], each = 26),
					 c(rep(letters, floor(k / 26)), letters[seq(1, k %% 26)]))
	raters <- raters[1:k]
	thedata <- matrix(NA, nEvents * k, nrow = nEvents, ncol = k)
	dimnames(thedata) <- list(1:nEvents, raters)
	for(i in 1:nrow(thedata)) {
		l <- base::sample(nLevels, prob = response.probs)
		thedata[i, (i %% k) + 1] <- l[1]
		if(runif(1) <= agree) {
			thedata[i, -((i %% k) + 1)] <- l[1]
		} else {
			# probs <- c(agree, rep((1 - agree) / (nLevels - 1), times = (nLevels - 1)))
			# thedata[i, -((i %% k) + 1)] <- base::sample(l, size = k - 1,
			# 											replace = TRUE, prob = probs)
			thedata[i, -((i %% k) + 1)] <- base::sample(l,
														size = k - 1,
														replace = TRUE,
														prob = response.probs)
		}
	}
	if(k_per_event < k) {
		thedata <- t(apply(thedata, 1, FUN = function(x) {
			x[-sample(k, size = k_per_event)] <- NA; return(x)
		}))
	}
	return(thedata)
}

#' Simulate a rating matrix with two ratings per event.
#'
#' This is a alternative implementation of the \code{\link{simulateRatingMatrix}}
#' function for when each event is has two ratings from k available raters.
#' For each event, one rater is randomly selected and given a rating from the
#' distribution defined by the \code{response.probs} parameter. A second randomly
#' selected rater will have the same rating with a probability defined by the
#'  \code{agree} parameter.
#'
#' @inheritParams simulateRatingMatrix
#' @export
simulateRatingMatrix2 <- function(nLevels, k, agree, nEvents = 100,
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

