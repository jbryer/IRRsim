library(IRRsim)
context('Testing simulateRatingMatrix')

test_that("simulating matrices", {
	set.seed(2112)
	test1 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000)

	expect_equal_to_reference(test1, file = 'ratingmatrix1')

	set.seed(2112)
	test2 <- simulateRatingMatrix(nLevels = 3, k = 6, agree = 0.6, nEvents = 1000,
								  response.probs = c(.1, .3, .6))

	expect_equal_to_reference(test2, file = 'ratingmatrix2')

	test2.tab <- prop.table(table(test2))
	expect_lt(test2.tab[1], 0.2)
	expect_lt(test2.tab[2], 0.4)
	expect_gt(test2.tab[3], 0.5)
})
