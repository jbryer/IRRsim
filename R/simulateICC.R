#' Calculates intraclass correlations (ICC) for simulated samples of raters and
#' evaluations.
#'
#' @param nRaters the number of available raters
#' @param nRatersPerEvent the number of ratings for each per scoring event.
#' @param nLevels the number of possible outcomes there are for each rating.
#' @param nSamples the number of sample matrices to estimate at each agreement level.
#' @param nEvents the number of rating events within each matrix.
#' @param agreements vector of percent agreements to simulate.
#' @param response.probs probability weights for the distribution of scores.
#'        See \code{\link{simulateRatingMatrix}} for more information.
#' @param parallel whether to simulated the data using multiple cores.
#' @param numCores number of cores to use if the simulation is run in parallel.
#' @param ... other parameters.
#' @return a list of length \code{nSamples * length(nRaters) * length(agreements)}.
#'        Each element of the list represents one simulation with the following
#'        values: \describe{
#'        \item{k}{the number of raters used in the simulation.}
#'        \item{simAgreement}{the calculated percent agreement from the sample.}
#'        \item{agreement}{the specified percent agreement used for drawing the random sample.}
#'        \item{skewness}{skewness of all responses.}
#'        \item{kurtosis}{Kurtosis for all responses.}
#'        \item{MaxResponseDiff}{the difference between the most and least freqeuent responses.}
#'        \item{ICC1}{ICC1 as described in Shrout and Fleiss (1979)}
#'        \item{ICC2}{ICC2 as described in Shrout and Fleiss (1979)}
#'        \item{ICC3}{ICC3 as described in Shrout and Fleiss (1979)}
#'        \item{ICC1k}{ICC1k as described in Shrout and Fleiss (1979)}
#'        \item{ICC2k}{ICC2k as described in Shrout and Fleiss (1979)}
#'        \item{ICC3k}{ICC3k as described in Shrout and Fleiss (1979)}
#'        \item{Fleiss_Kappa}{Fleiss' Kappa for m raters as described in Fleiss (1971).}
#'        \item{Cohen_Kappa}{Cohen's Kappa as calculated in psych::cohen.kappa. Note that this
#'              calculated for all datasets even though it is only appropriate for two raters.}
#'        \item{data}{The simulated matrix}
#'        }
#' @seealso as.data.frame.IRRsim
#' @export
#' @examples
#' icctest <- simulateICC(nLevels = 3, nRaters = 2, nSamples = 10, parallel = FALSE, showTextProgress = FALSE)
#' summary(icctest)
simulateICC <- function(nRaters = c(2),
						nRatersPerEvent = nRaters,
						nLevels = 4,
						nEvents = 100,
						nSamples = 100,
						agreements = seq(0.1, 0.9, by = 0.1),
						response.probs = rep(1 / nLevels, nLevels),
						showShinyProgress = FALSE,
						showTextProgress = !showShinyProgress,
						numCores = (parallel::detectCores() - 1),
						parallel = (numCores > 1),
						...) {
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
			k_per_event = rep(nRatersPerEvent, each = length(agreements) * nSamples),
			simAgreement = rep(rep(agreements, each = nSamples), length(nRaters)),
			stringsAsFactors = FALSE
		)

		simData <- NULL

		if(parallel) {
			cl <- snow::makeCluster(numCores)
			doSNOW::registerDoSNOW(cl)
			snow::clusterEvalQ(cl,suppressPackageStartupMessages(library(IRRsim)))
			opts <- list(progress = progress)

			tmp <- apply(tests, 1, FUN = function(X) {
				list(i = X['i'],
					 k = X['k'],
					 k_per_event = X['k_per_event'],
					 agree = X['simAgreement'])
			})

			simData <- foreach::foreach(params = tmp,
										.export = c('nLevels', 'nEvents', 'response.probs'),
										.options.snow = opts) %dopar% {
				test <- IRRsim::simulateRatingMatrix(nLevels = nLevels,
													 nEvents = nEvents,
													 k = params$k,
													 k_per_event = params$k_per_event,
													 agree = params$agree,
													 response.probs = response.probs)
				test2 <- as.integer(test)

				# Using DescTools package
				skew <- DescTools::Skew(test2, na.rm = TRUE)
				kurtosis <- DescTools::Kurt(test2, na.rm = TRUE)
				icc <- DescTools::ICC(test)
				icc.col <- 'est'
				ck <- NA
				if(params$k == 2 | params$k_per_event == 2) {
					tmp <- t(apply(test, 1, FUN = function(X) { X[!is.na(X)] }))
					ck <- DescTools::CohenKappa(tmp[,1], tmp[,2])
				}

				# Using psych package
				# skew <- psych::skew(test2, na.rm = TRUE)
				# kurtosis <- psych::kurtosi(test2, na.rm = TRUE)
				# icc <- psych::ICC(test)
				# icc.col <- 'ICC'
				# ck <- psych::cohen.kappa(tmp)$kappa

				MaxResponseDiff <- abs(max(diff(prop.table(table(test2)))))
			  	kf <- kappam.fleiss2(test)

			  	# NOTE: When adding IRR stats here, be sure to add them to
			  	# as.data.frame.IRRsim too!
				result <- list(index = unname(params$i),
							nLevels = nLevels,
							nEvents = nEvents,
							k = unname(params$k),
							k_per_event = unname(params$k_per_event),
							simAgreement = unname(params$agree),
							agreement = agreement(test),
							skewness = skew,
							kurtosis = kurtosis,
							MaxResponseDiff = MaxResponseDiff,
							ICC1 = icc$results['Single_raters_absolute',icc.col],
							ICC2 = icc$results['Single_random_raters',icc.col],
							ICC3 = icc$results['Single_fixed_raters',icc.col],
							ICC1k = icc$results['Average_raters_absolute',icc.col],
							ICC2k = icc$results['Average_random_raters',icc.col],
							ICC3k = icc$results['Average_fixed_raters',icc.col]
							# Fleiss_Kappa = kf$value
				)
				if(!is.na(ck)) {
					result$Cohen_Kappa <- ck
				}
				result$data <- test

				return(result)
			}

			snow::stopCluster(cl)
		} else {
			simData <- list()
			for(i in 1:nrow(tests)) {
				test <- IRRsim::simulateRatingMatrix(nLevels = nLevels,
													 nEvents = nEvents,
													 k = tests[i,]$k,
													 k_per_event = tests[i,]$k,
													 agree = tests[i,]$simAgreement,
													 response.probs = response.probs)
				test2 <- as.integer(test)

				# Using DescTools package
				skew <- DescTools::Skew(test2, na.rm = TRUE)
				kurtosis <- DescTools::Kurt(test2, na.rm = TRUE)
				icc <- DescTools::ICC(test)
				icc.col <- 'est'
				ck <- NA
				if(tests[i,]$k == 2 | tests[i,]$k_per_event == 2) {
					tmp <- t(apply(test, 1, FUN = function(X) { X[!is.na(X)] }))
					ck <- DescTools::CohenKappa(tmp[,1], tmp[,2])
				}

				# Using psych package
				# skew <- psych::skew(test2, na.rm = TRUE)
				# kurtosis <- psych::kurtosi(test2, na.rm = TRUE)
				# icc <- psych::ICC(test)
				# icc.col <- 'ICC'
				# ck <- psych::cohen.kappa(tmp)$kappa

				MaxResponseDiff <- abs(max(diff(prop.table(table(test2)))))
				kf <- kappam.fleiss2(test)

				# NOTE: When adding IRR stats here, be sure to add them to
				# as.data.frame.IRRsim too!
				simData[[i]] <- list(index = i,
									 nLevels = nLevels,
									 nEvents = nEvents,
									 k = tests[i,]$k,
									 k_per_event = tests[i,]$k_per_event,
									 simAgreement = tests[i,]$simAgreement,
									 agreement = agreement(test),
									 skewness = skew,
									 kurtosis = kurtosis,
									 MaxResponseDiff = MaxResponseDiff,
									 ICC1 = icc$results['Single_raters_absolute',icc.col],
									 ICC2 = icc$results['Single_random_raters',icc.col],
									 ICC3 = icc$results['Single_fixed_raters',icc.col],
									 ICC1k = icc$results['Average_raters_absolute',icc.col],
									 ICC2k = icc$results['Average_random_raters',icc.col],
									 ICC3k = icc$results['Average_fixed_raters',icc.col]
									 # Fleiss_Kappa = kf$value
				)
				if(!is.na(ck)) {
					simData[[i]]$Cohen_Kappa <- ck
				}
				simData[[i]]$data <- test
				progress()
			}
		}

		return(simData)
	}

	if(showShinyProgress) {
		simData <- withProgress(message = 'Simulating data',
							  min = 0, max = totalIterations, value = 0, simulate())
	} else {
		simData <- simulate()
	}

	if(showTextProgress) { close(pb) }

	class(simData) <- c('IRRsim', 'list')

	return(simData)
}
