library(devtools)

# Install dependencies (if necessary)
install_dev_deps()

# Documentation
document()
build_vignettes()
pkgdown::build_site()

# Install / Build package
install()
build(vignettes = FALSE)

library(IRRsim)
ls('package:IRRsim')

# Generate the data files used in the Vignette
source('data-raw/IRRguidelines.R') # Generate the IRRguidelines data file
source('data-raw/IRRsimulations.R') # WARNING! Will take a long time to run
tools::resaveRdaFiles('data')
data("IRRsimData")
data("IRRguidelines")

# Testing/getting ready for CRAN submission
test()
check(cran = TRUE)
# submit_cran()

library(RColorBrewer)
display.brewer.all()

################################################################################
# Basic usage
library(IRRsim)

vignette(package = 'IRRsim') # Make sure the vignette is listed
vignette('IRRsim') # Open the vignette

IRRsim_demo() # Run the shiny app

demo('IRRsim') # See demo/IRRsim.R for examples.


# Build prediction table
data('IRRsimData', package = 'IRRsim')
rsquared.df <- data.frame()
for(n in unique(IRRsimData$nLevels)) {
	tmp.n <- IRRsimData[IRRsimData$nLevels == n,]
	for(k in unique(tmp.n$k)) {
		tmp.k <- tmp.n[tmp.n$k == k,]
		for(k_per_event in unique(tmp.k$k_per_event)) {
			tmp <- tmp.k[tmp.k$k_per_event == k_per_event,]
			icc1.out <- lm(ICC1 ~ agreement + I(agreement^2), data = tmp)
			icc2.out <- lm(ICC2 ~ agreement + I(agreement^2), data = tmp)
			icc3.out <- lm(ICC3 ~ agreement + I(agreement^2), data = tmp)
			icc1k.out <- lm(ICC1k ~ agreement + I(agreement^2), data = tmp)
			icc2k.out <- lm(ICC2k ~ agreement + I(agreement^2), data = tmp)
			icc3k.out <- lm(ICC3k ~ agreement + I(agreement^2), data = tmp)
			rsquared.df <- rbind(rsquared.df, data.frame(
				nLevels = n,
				k = k,
				k_per_event = k_per_event,
				ICC1 = summary(icc1.out)$r.squared,
				ICC2 = summary(icc2.out)$r.squared,
				ICC3 = summary(icc3.out)$r.squared,
				ICC1k = summary(icc1k.out)$r.squared,
				ICC2k = summary(icc2k.out)$r.squared,
				ICC3k = summary(icc3k.out)$r.squared
			))
		}
	}
}
print(rsquared.df, digits = 2)
apply(rsquared.df[,-c(1:3)], 2, mean) # Average R-squared for each ICC

l <- loess(ICC1 ~ agreement, data = IRRsimData[IRRsimData$k == 2,])
summary(l)
plot(seq(0.01, 0.99, 0.01), predict(l, data.frame(agreement = seq(0.01, 0.99, 0.01))), type = 'l')

rsquared.df.melted <- melt(rsquared.df, id.vars = c('nLevels','k','k_per_event'),
						   variable.name = 'ICC', value.name = 'R-Squared')
# ggplot(rsquared.df.melted, aes(x = ))

# write.csv(rsquared.df, file = 'r-squared.csv', row.names = FALSE)


icc1.out <- lm(ICC1 ~ nLevels + k + k_per_event + agreement + I(agreement^2) + MaxResponseDiff,
			   data = IRRsimData)
summary(icc1.out)
icc2.out <- lm(ICC2 ~ nLevels + k + k_per_event + agreement + I(agreement^2) + MaxResponseDiff,
			   data = IRRsimData)
summary(icc2.out)
icc3.out <- lm(ICC3 ~ nLevels + k + k_per_event + agreement + I(agreement^2) + MaxResponseDiff,
			   data = IRRsimData)
summary(icc3.out)
icc1k.out <- lm(ICC1k ~ nLevels + k + k_per_event + agreement + I(agreement^2) + MaxResponseDiff,
				data = IRRsimData)
summary(icc1k.out)
icc2k.out <- lm(ICC2k ~ nLevels + k + k_per_event + agreement + I(agreement^2) + MaxResponseDiff,
				data = IRRsimData)
summary(icc2k.out)
icc3k.out <- lm(ICC3k ~ nLevels + k + k_per_event + agreement + I(agreement^2) + MaxResponseDiff,
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
print(t(df.summary), digits = 3)
