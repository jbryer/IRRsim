library(devtools)

# Install dependencies (if necessary)
devtools::install_dev_deps()

# Documentation
usethis::use_tidy_description()
devtools::document()
devtools::build_readme()
devtools::build_vignettes()
# file.copy('inst/slides/IRRsim-Presentation.html',
# 		  'docs/IRRsim-Presentation.html',
# 		  overwrite = TRUE) # copy the slides to the docs directory
rmarkdown::render('inst/slides/IRRsim-Presentation.Rmd',
				  output_format = 'ioslides_presentation')
# install.packages("webshot")
# webshot::install_phantomjs()
# webshot::rmdshot('inst/slides/IRRsim-Presentation.Rmd',
# 				 'inst/slides/IRRsim-Presentation.pdf',
# 				 vwidth = 992,
# 				 vheight = 744)
# rmarkdown::render('inst/slides/IRRsim-Presentation.Rmd',
# 				  output_format = 'pdf_document')

# Install / Build package
devtools::install()
devtools::build(vignettes = FALSE)


# Testing/getting ready for CRAN submission
devtools::test()
devtools::check(cran = TRUE)

library(IRRsim)
ls('package:IRRsim')




# Generate the data files used in the Vignette
source('data-raw/IRRguidelines.R') # Generate the IRRguidelines data file
source('data-raw/IRRsimulations.R') # WARNING! Will take a long time to run
tools::resaveRdaFiles('data')
# data("IRRsimData")
data("IRRguidelines")


tools::resaveRdaFiles('data/')

library(RColorBrewer)
display.brewer.all()

################################################################################
# Basic usage
library(IRRsim)

vignette(package = 'IRRsim') # Make sure the vignette is listed
vignette('IRRsim') # Open the vignette

IRRsim_demo() # Run the shiny app
LRsim_demo()

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

##### Hex Logo #################################################################
library(hexSticker)

p <- "private/people.png"
hexSticker::sticker(p,
					filename = 'man/figures/IRRsim.png',
					p_size = 16,
					package = 'IRRsim',
					url = "irrsim.bryer.org",
					# p_family = 'gochi',
					u_size = 9.0,
					s_width = .7, s_height = .7,
					s_x = 1, s_y = 1.15,
					p_x = 1, p_y = 0.45,
					p_color = "#8da0cb",
					h_fill = '#FFFFFF',
					h_color = '#8da0cb',
					u_color = '#a6d854',
					white_around_sticker = FALSE)
