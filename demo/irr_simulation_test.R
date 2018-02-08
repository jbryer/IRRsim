library(IRRsim)

test1 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000)
prop.table(table(as.integer(test1))) # Distribution of scores (default is uniform)

test2 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000,
							  response.probs = c(.1, .3, .6))
prop.table(table(as.integer(test2)))

##### Test with defaults
start <- Sys.time(); test1 <- simulateICC(parallel = TRUE, nSamples = 200); Sys.time() - start
start <- Sys.time(); test2 <- simulateICC(parallel = FALSE, nSamples = 200); Sys.time() - start

loess.out <- loess(ICC1 ~ agreement, data = test1)
predict(loess.out, newdata = seq(0.05, 0.95, by = 0.05), se = TRUE)


##### Simulating around 60% agreement
tests.3levels.60percent <- simulateICC(nRaters = 10, nLevels = 4, agreements = c(.6))
head(tests.3levels.60percent)
ggplot(tests.3levels.60percent, aes(x = agreement, y = ICC1)) + geom_point()

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
