## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)
library(IRRsim)
set.seed(2112) # For reproducibility

## ----testdata------------------------------------------------------------
test <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 10)
test

## ----agreementTest-------------------------------------------------------
agreement(test)

## ----simulate, cache = TRUE, message = FALSE, warning = FALSE, results = 'hide'----
tests.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3)
tests.5levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 5)
tests.9levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 9)

## ------------------------------------------------------------------------
ggplot(tests.3levels, aes(x = agreement)) + geom_density()
ggplot(tests.3levels, aes(x = ICC1)) + geom_density()

## ------------------------------------------------------------------------
ggplot(tests.3levels, aes(x = agreement, y = ICC1, color = factor(k))) + 
	geom_point(alpha = 0.3) + 
	geom_smooth(method = 'loess') + 
	scale_color_hue('n Raters') +
	xlim(c(0,1)) + ylim(c(-0.25,1)) +
	xlab('Percent Agreement') + ylab('ICC1') +
	ggtitle('ICC1 with 3 scoring levels')

ggplot(tests.5levels, aes(x = agreement, y = ICC1, color = factor(k))) + 
	geom_point(alpha = 0.3) + 
	geom_smooth(method = 'loess') + 
	scale_color_hue('n Raters') +
	xlim(c(0,1)) + ylim(c(-0.25,1)) +
	xlab('Percent Agreement') + ylab('ICC1') +
	ggtitle('ICC1 with 5 scoring levels')

ggplot(tests.9levels, aes(x = agreement, y = ICC1, color = factor(k))) + 
	geom_point(alpha = 0.3) + 
	geom_smooth(method = 'loess') + 
	scale_color_hue('n Raters') +
	xlim(c(0,1)) + ylim(c(-0.25,1)) +
	xlab('Percent Agreement') + ylab('ICC1') +
	ggtitle('ICC1 with 9 scoring levels')


## ------------------------------------------------------------------------
tmp <- melt(tests.3levels[,c('i','k','agreement','ICC1','ICC2','ICC3','ICC1k','ICC2k','ICC3k')], 
			id.vars = c('i','k','agreement'))
ggplot(tmp, aes(x = agreement, y = value, color = factor(k))) + 
	geom_smooth(method = 'loess') + 
	facet_wrap(~ variable) +
	scale_color_hue('n Raters') +
	xlim(c(0,1)) +
	xlab('Percent Agreement') + ylab('ICC') +
	ggtitle('ICC and Percent Rater Agreement (3 scoring levels)')

## ------------------------------------------------------------------------
lm.out3 <- lm(ICC1 ~ I(agreement ^ 2), data = tests.3levels)
summary(lm.out3)

lm.out5 <- lm(ICC1 ~ I(agreement ^ 2), data = tests.5levels)
summary(lm.out5)

lm.out9 <- lm(ICC1 ~ I(agreement ^ 2), data = tests.9levels)
summary(lm.out9)

