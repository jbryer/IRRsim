#' Convert IRR simulation run to a data frame.
#'
#' Converts the results of [IRRsim::simulateIRR()] to a `data.frame`.
#'
#' @param x the result of \code{\link{simulateIRR}}.
#' @param ... currently unused.
#' @return a data.frame with the following columns:
#' \describe{
#' \item{nLevels}{}
#' \item{nEvents}{}
#' \item{k}{the number of raters used in the simulation.}
#' \item{k_per_event}{number of raters per scoring event}
#' \item{simAgreement}{the calculated percent agreement from the sample.}
#' \item{agreement}{the specified percent agreement used for drawing the random sample.}
#' \item{skewness}{skewness of all responses.}
#' \item{kurtosis}{Kurtosis for all responses.}
#' \item{MaxResponseDiff}{the difference between the most and least freqeuent responses.}
#' \item{ICC1}{ICC1 as described in Shrout and Fleiss (1979)}
#' \item{ICC2}{ICC2 as described in Shrout and Fleiss (1979)}
#' \item{ICC3}{ICC3 as described in Shrout and Fleiss (1979)}
#' \item{ICC1k}{ICC1k as described in Shrout and Fleiss (1979)}
#' \item{ICC2k}{ICC2k as described in Shrout and Fleiss (1979)}
#' \item{ICC3k}{ICC3k as described in Shrout and Fleiss (1979)}
#' \item{Fleiss_Kappa}{Fleiss' Kappa for m raters as described in Fleiss (1971).}
#' \item{Cohen_Kappa}{Cohen's Kappa as calculated in psych::cohen.kappa. Note that this
#'       calculated for all datasets even though it is only appropriate for two raters.}
#' }
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
