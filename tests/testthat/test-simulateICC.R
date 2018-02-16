library(IRRsim)
context("Testing simulateICC")

skip_on_cran()

test_that("simulateICC", {
	set.seed(2112)
	start <- Sys.time(); test1 <- simulateICC(parallel = TRUE, nSamples = 50, nLevels = 3, nRaters = 6); test1.time <- Sys.time() - start
	start <- Sys.time(); test2 <- simulateICC(parallel = FALSE, nSamples = 50, nLevels = 3, nRaters = 6); test2.time <- Sys.time() - start

	expect_lt(test1.time, test2.time)

	expect_is(test1, 'IRRsim')
	expect_is(test2, 'IRRsim')
	expect_equal_to_reference(test1, file = 'simulateICC1')
	expect_equal_to_reference(test2, file = 'simulateICC2')
})

test_that("simulateICC with multiple raters", {
	tests.3levels <- simulateICC(nRaters = c(6, 9, 12), nLevels = 3, nSamples = 50)
	expect_equal_to_reference(tests.3levels, file = 'simulateICC3')
})
