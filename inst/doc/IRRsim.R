## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(IRRsim)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)
set.seed(2112) # For reproducibility

## ----testdata------------------------------------------------------------
test <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 10)
test

## ----agreementTest-------------------------------------------------------
agreement(test)

## ----simulate, cache = FALSE, message = FALSE, warning = FALSE, results = 'hide'----
tests.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3)
tests.5levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 5)
tests.9levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 9)

## ----dataframe-----------------------------------------------------------
test.3levels.df <- as.data.frame(tests.3levels)
dim(test.3levels.df)
head(test.3levels.df)

## ----ploticc1------------------------------------------------------------
plot(tests.3levels, stat = 'ICC1')
plot(tests.5levels, stat = 'ICC1')
plot(tests.9levels, stat = 'ICC1')

## ----plotall-------------------------------------------------------------
plot(tests.3levels)

## ----summaryfun----------------------------------------------------------
tests.3levels.sum <- summary(tests.3levels, stat = 'ICC1', method = 'quadratic')
summary(tests.3levels.sum$model[[1]]) # k = 6 raters
summary(tests.3levels.sum$model[[2]]) # k = 9 raters
summary(tests.3levels.sum$model[[3]]) # k = 12 raters

