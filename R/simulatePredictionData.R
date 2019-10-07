#' Simulate data to test logistic regression.
#'
#' @param mean1 the mean of group 1.
#' @param n1 the size of group 1.
#' @param mean2 the mean of group 2.
#' @param n2 the size of group 2.
#' @param mean_unknown the mean of the group with a random outcome.
#' @param n_unknown the size of the group with a random outcome.
#' @param ratio_unknown the probability of outcome 1 occurring in the random group.
#' @param sd the standard deviation for each group (i.e. parameter passed to
#'        \code{rnorm}). Variance is assumed to be equal across all groups.
#' @param group_labels the labels for the groups in the \code{data.frame}.
#' @return a \code{data.frame} with \code{n1 + n2 + n_unknown} rows and 3
#'        variables (\code{Group}, \code{X}, and \code{Y}).
#' @export
simulatePredictionData <- function(mean1 = -0.1,
								   n1 = 100,
								   mean2 = -mean1,
								   n2 = n1,
								   mean_unknown = (mean1 + mean2) / 2,
								   n_unknown = round(n1 / 2),
								   ratio_unknown = 0.5,
								   sd = 0.2,
								   group_labels = c("A", "B", "NA")) {
	df <- data.frame(
		Group = c(rep(group_labels[1], n1),
				  rep(group_labels[2], n2),
				  rep(group_labels[3], n_unknown)),
		Y = c(rep(0, n1),
			  rep(1, n2),
			  rep(sample(0:1,
			  		   prob = c(ratio_unknown, 1 - ratio_unknown),
			  		   size = n_unknown, replace = TRUE))),
		X = c(
			rnorm(n1, mean = mean1, sd = sd),
			rnorm(n2, mean = mean2, sd = sd),
			rnorm(n_unknown, mean = mean_unknown, sd = sd)
		),
		stringsAsFactors = FALSE
	)
	class(df) <- c('lrdf', 'data.frame')
	attr(df, 'mean1') <- mean1
	attr(df, 'n1') <- n1
	attr(df, 'mean2') <- mean2
	attr(df, 'n2') <- n2
	attr(df, 'mean_unknown') <- mean_unknown
	attr(df, 'n_unknown') <- n_unknown
	attr(df, 'ratio_unknown') <- ratio_unknown
	attr(df, 'sd') <- sd
	attr(df, 'labels') <- group_labels
	return(df)
}

if(FALSE) { # Test TODO: use testthat
	test1 <- simulatePredictionData()
	class(test1)
	head(test1)
	attributes(test1)
	plot(test1)
	table(test1$Group, test1$Y, useNA = 'ifany')
	test2 <- simulatePredictionData(n_unknown = 0)
	table(test2$Group, useNA = 'ifany')
}
