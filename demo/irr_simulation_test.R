library(IRRsim)

test1 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000)
prop.table(table(as.integer(test1))) # Distribution of scores (default is uniform)

test2 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000,
							  response.probs = c(.1, .3, .6))
prop.table(table(as.integer(test2)))


##### Simulating around 60% agreement
tests.3levels.50percent <- simulateICC(nRaters = 10, nLevels = 4, agreements = c(.6))
head(tests.3levels.50percent)
ggplot(tests.3levels.50percent, aes(x = agreement, y = ICC1)) + geom_point()

##### Uniform response distribution
tests.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3)
ggplot(tests.3levels, aes(x = agreement)) + geom_density()
ggplot(tests.3levels, aes(x = ICC1)) + geom_density()

ggplot(tests.3levels, aes(x = agreement, y = ICC1, color = factor(k))) +
	geom_point(alpha = 0.3) +
	geom_smooth(method = 'loess') +
	scale_color_hue('n Raters') +
	xlim(c(0,1)) + ylim(c(-0.25,1)) +
	xlab('Percent Agreement') + ylab('ICC1') +
	ggtitle('ICC1 with 3 scoring levels')

tests.melted <- melt(tests.3levels, id.vars = c('i', 'k', 'simAgreement', 'agreement'))
ggplot(tests.melted, aes(x = agreement, y = value, color = factor(k))) +
	# geom_point(alpha = 0.3) +
	geom_smooth(method = 'loess') +
	scale_color_hue('n Raters') +
	facet_wrap(~ variable) +
	xlim(c(0,1)) + #ylim(c(-0.25,1)) +
	xlab('Percent Agreement') + ylab('ICC') +
	ggtitle('ICC with 3 scoring levels')

##### Unequal response distributions
tests2.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3, response.probs = c(.1, .1, .8))

ggplot(tests2.3levels, aes(x = agreement, y = ICC1, color = factor(k))) +
	geom_point(alpha = 0.3) +
	geom_smooth(method = 'loess') +
	scale_color_hue('n Raters') +
	xlim(c(0,1)) + ylim(c(-0.25,1)) +
	xlab('Percent Agreement') + ylab('ICC1') +
	ggtitle('ICC1 with 3 scoring levels (unequal response distribution)')
