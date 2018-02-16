library(IRRsim)
context('Testing simulateRatingMatrix')

test_that("simulating matrices", {
	expect_equal_to_reference({
		set.seed(2112)
		simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000)
	}, file = 'ratingmatrix1')

	expect_equal_to_reference({
		set.seed(2112)
		simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000,
							 response.probs = c(.1, .3, .6))

	}, file = 'ratingmatrix2')
})
