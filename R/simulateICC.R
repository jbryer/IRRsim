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
#' @param parallel whether to simulated the data using multiple cores.
#' @param numCores number of cores to use if the simulation is run in parallel.
#' @return a data.frame with the following columns: \describe{
#'        \item{k}{the number of raters used in the simulation.}
#'        \item{simAgreement}{the calculated percent agreement from the sample.}
#'        \item{agreement}{the specified percent agreement used for drawing the random sample.}
#'        \item{ICC1}{ICC1 as described in Shrout and Fleiss (1979)}
#'        \item{ICC2}{ICC2 as described in Shrout and Fleiss (1979)}
#'        \item{ICC3}{ICC3 as described in Shrout and Fleiss (1979)}
#'        \item{ICC1k}{ICC1k as described in Shrout and Fleiss (1979)}
#'        \item{ICC2k}{ICC2k as described in Shrout and Fleiss (1979)}
#'        \item{ICC3k}{ICC3k as described in Shrout and Fleiss (1979)}
#'        }
#' @export
simulateICC <- function(nRaters = c(2),
						nLevels = 3,
						nEvents = 100,
						nSamples = 100,
						agreements = seq(0.1, 0.9, by = 0.1),
						response.probs = rep(1 / nLevels, nLevels),
						showTextProgress = !showShinyProgress,
						showShinyProgress = FALSE,
						parallel = (numCores > 1),
						numCores = (parallel::detectCores() - 1) ) {

	totalIterations <- nSamples * length(nRaters) * length(agreements)

	if(showTextProgress) {
		pb <- txtProgressBar(style = 3, min = 0, max = totalIterations)
	}

	progress <- function(...) {
		if(showTextProgress) { setTxtProgressBar(pb, getTxtProgressBar(pb) + 1) }
		if(showShinyProgress) { incProgress(1) }
	}

	simulate <- function() { # Create function so we can optionally wrap in shiny::withProgress
		tests <- data.frame(
			i = rep(1:nSamples, length(nRaters) * length(agreements)),
			k = rep(nRaters, each = length(agreements) * nSamples),
			simAgreement = rep(rep(agreements, each = nSamples), length(nRaters)),
			agreement = rep(NA_integer_, totalIterations),
			ICC1 = rep(NA_integer_, totalIterations),
			ICC2 = rep(NA_integer_, totalIterations),
			ICC3 = rep(NA_integer_, totalIterations),
			ICC1k = rep(NA_integer_, totalIterations),
			ICC2k = rep(NA_integer_, totalIterations),
			ICC3k = rep(NA_integer_, totalIterations),
			stringsAsFactors = FALSE
		)

		if(parallel) {
			no_cores <- parallel::detectCores() - 1
			cl <- snow::makeCluster(no_cores)
			doSNOW::registerDoSNOW(cl)
			snow::clusterEvalQ(cl,library(IRRsim))
			opts <- list(progress = progress)

			tmp <- apply(tests, 1, FUN = function(X) { list(k = X['k'], agree = X['simAgreement'])})

			tests <- foreach::foreach(params = tmp,
									  .combine = rbind,
									  .export = c('nLevels', 'nEvents', 'response.probs'),
									  .options.snow = opts) %dopar% {
				test <- IRRsim::simulateRatingMatrix(nLevels = nLevels,
											 nEvents = nEvents,
											 k = params$k,
											 agree = params$agree,
											 response.probs = response.probs)
				icc <- DescTools::ICC(test)

				c(k = unname(params$k),
				  simAgreement = unname(params$agree),
				  agreement = agreement(test),
				  ICC1 = icc$results[1,]$est,
				  ICC2 = icc$results[2,]$est,
				  ICC3 = icc$results[3,]$est,
				  ICC1k = icc$results[4,]$est,
				  ICC2k = icc$results[5,]$est,
				  ICC3k = icc$results[6,]$est)
			}

			snow::stopCluster(cl)

			row.names(tests) <- 1:nrow(tests)
			tests <- as.data.frame(tests)
		} else {
			for(i in 1:nrow(tests)) {
				test <- IRRsim::simulateRatingMatrix(nLevels = nLevels,
													 nEvents = nEvents,
													 k = tests[i,]$k,
													 agree = tests[i,]$simAgreement,
													 response.probs = response.probs)
				icc <- DescTools::ICC(test)

				tests[i,]$agreement <- IRRsim::agreement(test)
				tests[i,]$ICC1 <- icc$results[1,]$est
				tests[i,]$ICC2 <- icc$results[2,]$est
				tests[i,]$ICC3 <- icc$results[3,]$est
				tests[i,]$ICC1k <- icc$results[4,]$est
				tests[i,]$ICC2k <- icc$results[5,]$est
				tests[i,]$ICC3k <- icc$results[6,]$est

				progress()
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
