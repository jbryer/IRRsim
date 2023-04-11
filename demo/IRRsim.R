library(IRRsim)

# Guidelines for interpreting IRR
data(IRRguidelines)

set.seed(2112)

# How Cohen's kappa is calculated with m > 2 raters. Note that Cohen's kappa is only appropriate
# for two raters.
test <- simulateRatingMatrix(nLevels = 4, k = 6, k_per_event = 2, agree = 0.6, nEvents = 100)
print(head(test), na.print = '')
agreement(test)
tmp <- t(apply(test, 1, FUN = function(X) { X[!is.na(X)] }))
DescTools::CohenKappa(tmp[,1], tmp[,2])
# How the ICC stats are calculated
DescTools::ICC(test)

nLevels <- 4
k <- 10
agree <- .7
nEvents <- 100
thedata <- simulateRatingMatrix(nLevels, k, k_per_event = 3, agree)
head(thedata)
irr::agree(thedata)
agreement(thedata)

##### Simple test (from the examples)
icctest <- simulateIRR(nLevels = 3, nRaters = 2, nSamples = 10, parallel = FALSE, showTextProgress = FALSE)
icctest
head(as.data.frame(icctest))

##### Test with 6 raters with 3 response levels
# Simulate in parallel
start <- Sys.time(); test1 <- simulateIRR(parallel = TRUE, nSamples = 200, nLevels = 3, nRaters = 6); Sys.time() - start
# Disable parallel processing
# start <- Sys.time(); test2 <- simulateIRR(parallel = FALSE, nSamples = 200, nLevels = 3, nRaters = 6); Sys.time() - start

# Summary and plot of all IRR stats
summary(test1)
plot(test1, point.alpha = 0.1, method = 'none')

# Summary and plot of ICC1 only
icc1.summary <- summary(test1, stat = 'ICC1', method = 'quadratic')
icc1.summary
summary(icc1.summary$model)
plot(test1, stat = 'ICC1')

# Get the distribution of scores
tmp <- sapply(test1, FUN = function(x) { as.integer(x$data) })
prop.table(table(tmp))


##### Uniform response distribution
test1.3levels <- simulateIRR(nRaters = c(6, 9, 12), nLevels = 3)
test1.3levels.df <- as.data.frame(test1.3levels)
# Check the actual distribution
tmp <- unlist(sapply(test1.3levels, FUN = function(x) { as.integer(x$data) }))
prop.table(table(tmp))

plot(test1.3levels, stat = 'ICC1')


##### Unequal response distributions (moderately skewed)
test2.3levels <- simulateIRR(nRaters = c(6, 9, 12), nLevels = 3, response.probs = c(.2, .2, .6))
test2.3levels.df <- as.data.frame(test2.3levels)
# Check the actual distribution
tmp <- unlist(sapply(test2.3levels, FUN = function(x) { as.integer(x$data) }))
prop.table(table(tmp))

plot(test2.3levels, stat = 'ICC1')


##### Unequal response distributions (highly skewed)
test3.3levels <- simulateIRR(nRaters = c(6, 9, 12), nLevels = 3, response.probs = c(.1, .1, .8))
test3.3levels.df <- as.data.frame(test3.3levels)
# Check the actual distribution
tmp <- unlist(sapply(test3.3levels, FUN = function(x) { as.integer(x$data) }))
prop.table(table(tmp))

plot(test3.3levels, stat = 'ICC1')


##### Unequal response distribution (lightly skewed)
test4.3levels <- simulateIRR(nRaters = c(6, 9, 12), nLevels = 3, response.probs = c(.275, .275, .45))
test4.3levels.df <- as.data.frame(test4.3levels)
# Check the actual distribution
tmp <- unlist(sapply(test4.3levels, FUN = function(x) { as.integer(x$data) }))
prop.table(table(tmp))

plot(test4.3levels, stat = 'ICC1')


# Combine the different response distributions
test1.3levels.df$ResponseDist <- 'Uniform'
test2.3levels.df$ResponseDist <- 'Moderately Skewed'
test3.3levels.df$ResponseDist <- 'Highly Skewed'
test4.3levels.df$ResponseDist <- 'Ligthly Skewed'
test.3levels.df <- rbind(test1.3levels.df, test2.3levels.df, test3.3levels.df, test4.3levels.df)


ggplot(test.3levels.df, aes(x = agreement, y = ICC1, color = ResponseDist)) +
	geom_point(alpha = 0.1) +
	geom_smooth(method = 'loess', formula = y ~ x, se = FALSE) +
	facet_wrap(~ factor(k))

ggplot(test.3levels.df, aes(x = agreement, y = ICC1, color = ResponseDist, linetype = factor(k))) +
	# geom_point(alpha = 0.1) +
	geom_hline(yintercept = c(0.4, 0.6, 0.75), alpha = 0.5) +
	# geom_vline(xintercept = c(0.7), alpha = 0.5) +
	geom_smooth(method = 'loess', formula = y ~ x, se = FALSE) +
	scale_linetype('n Raters') +
	xlab('Percent Agreement') +
	ggtitle('ICC1 vs Percent Agreement')


ggplot(test.3levels.df, aes(x = agreement, y = ICC1k, color = ResponseDist, linetype = factor(k))) +
	# geom_point(alpha = 0.1) +
	geom_hline(yintercept = c(0.4, 0.6, 0.75), alpha = 0.5) +
	# geom_vline(xintercept = c(0.7), alpha = 0.5) +
	geom_smooth(method = 'loess', formula = y ~ x, se = FALSE) +
	scale_linetype('n Raters') +
	xlab('Percent Agreement') +
	ggtitle('ICC1k vs Percent Agreement')
