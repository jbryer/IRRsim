library(IRRsim)

# Set run_parallel = FALSE to ensure subsequent runs generate the same data.
run_parallel <- TRUE
set.seed(2112)
# nRaters = 2:12  # Number of raters
nRaters <- c(2, 4, 8, 16)
nLevels <- 2:5  # The number of scoring levels
IRRsimulations <- list()
for(i in nLevels) {
	probability.weights <- list(
		'Uniform' = rep(1/i, i),
		'Lightly' = c(rep(.55 / (i - 1), i - 1), .55),
		'Moderately' = c(rep(.4 / (i - 1), i - 1), .6),
		'Highly' = c(rep(.2 / (i - 1), i - 1), .8)
	)

	for(j in seq_len(length(probability.weights))) {
		print(paste0('Simulating ', i, ' levels with ',
					 names(probability.weights)[j], ' distribution'))
		for(k in nRaters) {
			for(k_per_event in seq(2, k)) {
				tmp <- simulateIRR(nRaters = k,
								   nRatersPerEvent = k_per_event,
								   nLevels = i,
								   response.probs = probability.weights[[j]],
								   parallel = run_parallel,
								   showTextProgress = FALSE)
				tmp2 <- unlist(sapply(tmp, FUN = function(x) { as.integer(x$data) }))
				IRRsimulations[[length(IRRsimulations) + 1]] <- list(
					raw = tmp,
					nLevels = i,
					nRaters = k,
					nRatersPerEvent = k_per_event,
					response.probs.name = names(probability.weights)[j],
					response.probs = probability.weights[[j]],
					response.dist = prop.table(table(tmp2))
				)
			}
		}
	}
}
save(IRRsimulations, file = 'data-raw/IRRsimulations.rda')

# Create a data.frame combining the results of the simulations
load('data-raw/IRRsimulations.rda')
IRRsimData <- data.frame()
for(i in seq_len(length(IRRsimulations))) {
	tmp <- as.data.frame(IRRsimulations[[i]]$raw)
	tmp <- tmp[,!names(tmp) %in% c('Cohen_Kappa')]
	# tmp$ResponseDist <- IRRsimulations[[i]]$response.probs.name
	IRRsimData <- rbind(IRRsimData, tmp)
}
IRRsimData$nLevels <- factor(IRRsimData$nLevels, ordered = TRUE)
IRRsimData$PercentRated <- IRRsimData$k_per_event / IRRsimData$k
IRRsimData$k <- factor(IRRsimData$k, ordered = TRUE)
IRRsimData$k_per_event <- factor(IRRsimData$k_per_event, ordered = TRUE)
IRRsimData$ResponseDist <- cut(IRRsimData$MaxResponseDiff,
							   breaks = c(-Inf, 0.1, 0.2, 0.3, Inf),
							   labels = c('Uniform', 'Lightly Skewed', 'Moderately Skewed', 'Highly Skewed'),
							   ordered = TRUE)
save(IRRsimData, file = 'data-raw/IRRsimData.rda')

tools::resaveRdaFiles('data-raw/')

rm(IRRsimulations)
