# This script generates the results for a NCME 2019 conference proposal.
library(IRRsim)
data("IRRguidelines")

set.seed(2112) # For reproducibility

##### Example rating matrix
test <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 100)
print(head(test), na.print = '')

##### Example 1: 3 scoring levels with 6 raters
test1 <- simulateIRR(parallel = TRUE,# nSamples = 200,
					 nLevels = 3,
					 nRaters = 6, nRatersPerEvent = 2)
icc1.summary <- summary(test1, stat = 'ICC1', method = 'quadratic')
summary(icc1.summary$model)

# Calculate the corresponding percent rater agreement for Cicchetti's guidelines
newdata = data.frame(agreement = seq(0.01, 1, 0.01))
predictions <- predict(icc1.summary$model, newdata = newdata)
tab <- data.frame(Label = paste0(IRRguidelines[['Cicchetti']], ' "',
								 names(IRRguidelines[['Cicchetti']]), '"'),
				  ICC = IRRguidelines[['Cicchetti']],
				  Agreement = sapply(IRRguidelines[['Cicchetti']],
				  				   FUN = function(x) {
				  				   	min(which(predictions >= x)) / 100 }))

# Figure 1
plot(test1, stat = 'ICC1', method = 'quadratic') +
	geom_segment(data = tab, color = 'black', x = -Inf,
				 aes(y = ICC, yend = ICC, xend = Agreement)) +
	geom_segment(data = tab, color = 'black', y = -Inf,
				 aes(x = Agreement, xend = Agreement, yend = ICC)) +
	geom_text(data = tab, aes(x = 0, y = ICC, label = Label),
			  color = 'black', vjust = -0.5, size = 3, hjust = 'left') +
	geom_text(data = tab, aes(x = Agreement, y = min(predictions),
							  label = paste0(round(Agreement*100), '%')),
			  color = 'black', size = 3, hjust = -0.1)
ggsave('NCME1.png', width = 13, height = 8)


# Calculate the corresponding percent rater agreement for Shrout's guidelines
newdata = data.frame(agreement = seq(min(icc1.summary$data$agreement),
									 max(icc1.summary$data$agreement), 0.01))
predictions <- predict(icc1.summary$model, newdata = newdata)
tab <- data.frame(Label = paste0(IRRguidelines[['Shrout']], ' "',
								 names(IRRguidelines[['Shrout']]), '"'),
				  ICC = IRRguidelines[['Shrout']],
				  Agreement = sapply(IRRguidelines[['Shrout']],
				  				   FUN = function(x) {
				  				   	  min(newdata$agreement[predictions >= x])
				  				   }))
plot(test1, stat = 'ICC1', method = 'quadratic') +
	geom_segment(data = tab, color = 'black', x = -Inf,
				 aes(y = ICC, yend = ICC, xend = Agreement)) +
	geom_segment(data = tab, color = 'black', y = -Inf,
				 aes(x = Agreement, xend = Agreement, yend = ICC)) +
	geom_text(data = tab, aes(x = 0, y = ICC, label = Label),
			  color = 'black', vjust = -0.5, size = 3, hjust = 'left') +
	geom_text(data = tab, aes(x = Agreement, y = min(predictions),
							  label = paste0(round(Agreement*100), '%')),
			  color = 'black', size = 3, hjust = -0.1)


##### Example 2: 3 scoring levels with 2, 4, 8, and 16 raters
test2 <- simulateIRR(parallel = FALSE, nSamples = 200, nLevels = 3,
					 nRaters = c(2, 4, 8, 16))

icc2.summary <- summary(test2, stat = 'ICC1', method = 'quadratic')

# Add Cicchetti's guidelines
guide <- data.frame(label = paste0(IRRguidelines[['Cicchetti']], ' "',
								   names(IRRguidelines[['Cicchetti']]), '"'),
					y = IRRguidelines[['Cicchetti']])

# Add Schrout's guidelines
# guide <- data.frame(label = paste0(IRRguidelines[['Cicchetti']], ' "',
# 								   names(IRRguidelines[['Cicchetti']]), '"'),
# 					y = IRRguidelines[['Schrout']])

# Figure 2
plot(test2, stat = 'ICC1', method = 'quadratic', point.alpha = 0.05) +
	geom_hline(yintercept = IRRguidelines[['Cicchetti']]) +
	geom_text(data = guide, aes(x = 0, y = y, label = label),
			  color = 'black', vjust = -0.5, size = 3, hjust = 'left')
ggsave('NCME2.png', width = 13, height = 8)


# What is the PRA for Cicchetti's "good" reliability
newdata = data.frame(agreement = seq(0.01, 1, 0.01))
predictions <- predict(icc2.summary$model[[4]], newdata = newdata)
min(which(predictions > 0.4))


