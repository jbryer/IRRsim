library(IRRsim)

# Test specifying the response distributions
test1 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000)
prop.table(table(as.integer(test1))) # Distribution of scores (default is uniform)

test2 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000,
							  response.probs = c(.1, .3, .6))
prop.table(table(as.integer(test2)))

# How Cohen's kappa is calculated with m > 2 raters. Note that Cohen's kappa is only appropriate
# for two raters.
test <- simulateRatingMatrix(nLevels = 3, k = 2, agree = 0.6, nEvents = 100)
tmp <- t(apply(test, 1, FUN = function(X) { X[!is.na(X)] }))
DescTools::CohenKappa(tmp[,1], tmp[,2])
# How the ICC stats are calculated
DescTools::ICC(test)

##### Simple test (from the examples)
icctest <- simulateICC(nLevels = 3, nRaters = 2, nSamples = 10, parallel = FALSE, showTextProgress = FALSE)
icctest

##### Test with 6 raters with 3 response levels
start <- Sys.time(); test1 <- simulateICC(parallel = TRUE, nSamples = 200, nLevels = 3, nRaters = 6); Sys.time() - start
start <- Sys.time(); test2 <- simulateICC(parallel = FALSE, nSamples = 200, nLevels = 3, nRaters = 6); Sys.time() - start

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

test3 <- simulateICC(response.probs = c(.1, .3, .6))
tmp3 <- sapply(test3, FUN = function(x) { as.integer(x$data) })
prop.table(table(tmp3))



##### Uniform response distribution
tests.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3)
tests.3levels.df <- as.data.frame(tests.3levels)
head(tests.3levels.df)

sum.all <- summary(tests.3levels)
sum.all
sum.icc1 <- summary(tests.3levels, stat = 'ICC1')
sum.icc1

plot(tests.3levels)
plot(tests.3levels, stat = 'ICC1')

##### Unequal response distributions
tests2.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3, response.probs = c(.1, .1, .8))

plot(tests2.3levels)
