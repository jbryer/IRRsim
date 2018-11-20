library(devtools)

document()
build(vignettes = FALSE)
build_vignettes()
install()
test()
check(cran = TRUE)

pkgdown::build_site()

# Basic usage

library(IRRsim)

data("IRRsimData")
data("IRRguidelines")

vignette(package = 'IRRsim') # Make sure the vignette is listed
vignette('IRRsim') # Open the vignette

IRRsim_demo() # Run the shiny app

# See demo/IRRsim.R for examples.

################################################################################
# Generate the data files used in the Vignette
source('data-raw/IRRguidelines.R')


# Set run_parallel = TRUE to ensure subsequent runs generate the same data.
run_parallel <- FALSE
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
head(IRRsimData)
str(IRRsimData)
dim(IRRsimData)
save(IRRsimData, file = 'data/IRRsimData.rda')
rm(IRRsimulations)

load("data/IRRsimData.rda")

dim(IRRsimData)
head(IRRsimData)


ggplot(IRRsimData, aes(x = agreement, y = ICC1, color = ResponseDist)) +
	   	geom_smooth() +
	   	facet_wrap(~ k + k_per_event)


# Build prediction table

rsquared.df <- data.frame()
for(n in unique(IRRsimData$nLevels)) {
	for(k in unique(IRRsimData$k)) {
		tmp <- IRRsimData[IRRsimData$nLevels == n & IRRsimData$k == k,]
		icc1.out <- lm(ICC1 ~ agreement + I(agreement^2), data = tmp)
		icc2.out <- lm(ICC2 ~ agreement + I(agreement^2), data = tmp)
		icc3.out <- lm(ICC3 ~ agreement + I(agreement^2), data = tmp)
		icc1k.out <- lm(ICC1k ~ agreement + I(agreement^2), data = tmp)
		icc2k.out <- lm(ICC2k ~ agreement + I(agreement^2), data = tmp)
		icc3k.out <- lm(ICC3k ~ agreement + I(agreement^2), data = tmp)
		rsquared.df <- rbind(rsquared.df, data.frame(
			nLevels = n,
			k = k,
			ICC1 = summary(icc1.out)$r.squared,
			ICC2 = summary(icc2.out)$r.squared,
			ICC3 = summary(icc3.out)$r.squared,
			ICC1k = summary(icc1k.out)$r.squared,
			ICC2k = summary(icc2k.out)$r.squared,
			ICC3k = summary(icc3k.out)$r.squared
		))
	}
}
print(rsquared.df, digits = 2)
summary(rsquared.df)

write.csv(rsquared.df, file = 'r-squared.csv', row.names = FALSE)

icc1.out <- lm(ICC1 ~ nLevels + k + agreement + I(agreement^2) + MaxResponseDiff,
			   data = IRRsimData)
summary(icc1.out)
icc2.out <- lm(ICC2 ~ nLevels + k + agreement + I(agreement^2) + MaxResponseDiff,
			   data = IRRsimData)
summary(icc2.out)
icc3.out <- lm(ICC3 ~ nLevels + k + agreement + I(agreement^2) + MaxResponseDiff,
			   data = IRRsimData)
summary(icc3.out)
icc1k.out <- lm(ICC1k ~ nLevels + k + agreement + I(agreement^2) + MaxResponseDiff,
				data = IRRsimData)
summary(icc1k.out)
icc2k.out <- lm(ICC2k ~ nLevels + k + agreement + I(agreement^2) + MaxResponseDiff,
				data = IRRsimData)
summary(icc2k.out)
icc3k.out <- lm(ICC3k ~ nLevels + k + agreement + I(agreement^2) + MaxResponseDiff,
				data = IRRsimData)
summary(icc3k.out)

ls(summary(icc1.out))

df.summary <- rbind(
	c('r-squared' = summary(icc1.out)$r.squared, icc1.out$coefficients),
	c('r-squared' = summary(icc2.out)$r.squared, icc2.out$coefficients),
	c('r-squared' = summary(icc3.out)$r.squared, icc3.out$coefficients),
	c('r-squared' = summary(icc1k.out)$r.squared, icc1k.out$coefficients),
	c('r-squared' = summary(icc2k.out)$r.squared, icc2k.out$coefficients),
	c('r-squared' = summary(icc3k.out)$r.squared, icc3k.out$coefficients)
)
row.names(df.summary) <- c('ICC1', 'ICC2', 'ICC3', 'ICC1k', 'ICC2k', 'ICC3k')
print(t(df.summary), digits = 2)


lme.out1 <- lmer(ICC1 ~ agreement + I(agreement^2) + MaxResponseDiff + (1 | nLevels) + (1 | k), data = IRRsimData)
MuMIn::r.squaredGLMM(lme.out1)

lme.out1k <- lmer(ICC1k ~ agreement + I(agreement^2) + MaxResponseDiff + (1 | nLevels) + (1 | k), data = IRRsimData)
MuMIn::r.squaredGLMM(lme.out1k)
