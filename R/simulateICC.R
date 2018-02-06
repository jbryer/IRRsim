#' Calultes interclass correlations (ICC) for simulated samples of raters and
#' evaluations.
#'
#' @param nRaters the number of raters
#' @param nLevels the number of possible outcomes there are for each rating.
#' @param nSamples the number of sample matrices to estimate at each agreement level.
#' @param nEvents the number of rating events within each matrix.
#' @param agreements vector of percent agreements to simulate.
#' @param response.probs probability weights for the distribution of scores.
#'        See \code{\link{simulateRatingMatrix}} for more information.
#' @return a data.frame with the following columns:
#'        * \code{i} - index of the simulation (a integer between 1 and \code{nSamples})
#'        * \code{k} - the number of raters used in the simulation.
#'        * \code{simAgreement} - the calculated percent agreement from the sample.
#'        * \code{agreement} - the specified percent agreement used for drawing the random sample.
#'        * \code{ICC1} - ICC1 as described in Shrout and Fleiss (1979)
#'        * \code{ICC2} - ICC2 as described in Shrout and Fleiss (1979)
#'        * \code{ICC3} - ICC3 as described in Shrout and Fleiss (1979)
#'        * \code{ICC1k} - ICC1k as described in Shrout and Fleiss (1979)
#'        * \code{ICC2k} - ICC2k as described in Shrout and Fleiss (1979)
#'        * \code{ICC3k} - ICC3k as described in Shrout and Fleiss (1979)
#' @export
simulateICC <- function(nRaters = c(2),
						nLevels = 3,
						nEvents = 100,
						nSamples = 100,
						agreements = seq(0.1, 0.9, by = 0.1),
						response.probs = rep(1 / nLevels, nLevels),
						showTextProgress = !showShinyProgress,
						showShinyProgress = FALSE) {

	totalIterations <- nSamples * length(nRaters) * length(agreements)
	if(showTextProgress) {
		pb <- txtProgressBar(style = 3, min = 0, max = totalIterations)
	}

	simulate <- function() { # Create function so we can optionally wrap in shiny::withProgress
		tests <- data.frame()
		iter <- 0
		for(k in nRaters) {
			for(a in agreements) {
				for(i in 1:nSamples) {
					iter <- iter + 1
					test <- simulateRatingMatrix(nLevels = nLevels, nEvents = nEvents, k = k, agree = a,
												 response.probs = response.probs)
					icc <- DescTools::ICC(test)
					tests <- rbind(tests, data.frame(
						i = i,
						k = k,
						simAgreement = a,
						agreement = agreement(test),
						ICC1 = icc$results[1,]$est,
						ICC2 = icc$results[2,]$est,
						ICC3 = icc$results[3,]$est,
						ICC1k = icc$results[4,]$est,
						ICC2k = icc$results[5,]$est,
						ICC3k = icc$results[6,]$est,
						stringsAsFactors = FALSE
					))
					# Update progress bars
					if(showTextProgress) { setTxtProgressBar(pb, getTxtProgressBar(pb) + 1) }
					if(showShinyProgress) {
						incProgress(1,
									detail = paste0('k = ', k, '; agree = ', a))
					}
				}
			}
		}

		return(tests)
	}

	if(showShinyProgress) {
		tests <- withProgress(message = 'Simulating data',
							  min = 0, max = totalIterations, value = 0, simulate())
	} else {
		tests <- simulate()
	}

	if(showTextProgress) { close(pb) }

	return(tests)
}
