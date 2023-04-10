library(IRRsim)
context("Testing simulateIRR")

skip_on_cran()

test_that("simulateIRR", {
	set.seed(2112)
	start <- Sys.time(); test1 <- simulateIRR(parallel = TRUE, nSamples = 50, nLevels = 3, nRaters = 6); test1.time <- Sys.time() - start
	start <- Sys.time(); test2 <- simulateIRR(parallel = FALSE, nSamples = 50, nLevels = 3, nRaters = 6); test2.time <- Sys.time() - start

	# expect_lt(test1.time, test2.time)

	expect_is(test1, 'IRRsim')
	expect_is(test2, 'IRRsim')
	expect_equal_to_reference(test1, file = 'simulateIRR1')
	expect_equal_to_reference(test2, file = 'simulateIRR2')
})

test_that("simulateIRR with multiple raters", {
	set.seed(2112)
	tests.3levels <- simulateIRR(nRaters = c(6, 9, 12), nLevels = 3, nSamples = 50)
	expect_equal_to_reference(tests.3levels, file = 'simulateIRR3')
})
