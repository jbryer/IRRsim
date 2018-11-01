#' Plot the results of an IRR simulation.
#'
#' Returns a \code{ggplot2} expression.
#'
#' @param x the result of \code{\link{simulateICC}}.
#' @param method the method used to find predicted values. Possible values are
#'        loess, linear, and quadratic.
#' @param stat the IRR statistic to return summary for, or "all" for all metrics.
#' @param point.alpha the alpha used for points.
#' @param ... currently unused.
#' @export
plot.IRRsim <- function(x,
						method = 'loess',
						stat,
						point.alpha = 0.3,
						title = TRUE,
						...) {
	p <- NULL
	test <- as.data.frame(x)
	test$k <- as.character(test$k)
	if(missing(stat)) {
		tests.melted <- reshape2::melt(test, id.vars = c('nLevels', 'nEvents',
				'k', 'k_per_event', 'simAgreement', 'agreement',
				'skewness', 'kurtosis', 'MaxResponseDiff'))
		p <- ggplot(tests.melted, aes(x = agreement, y = value, color = k)) +
			geom_point(alpha = point.alpha) +
			facet_wrap(~ variable) +
			xlim(c(0,1)) +
			xlab('Percent Rater Agreement') + ylab('ICC')
		if(title) {
			km <- test$k_per_event
			p <- p + ggtitle(paste0('Inter-Rater Reliability with ', test[1,]$nLevels,
						   ' Scoring Levels and ',
						   test$k_per_event, ' out of ',
						   paste0(unique(test$k), collapse = ', '),
									' Raters per Scoring Event'))
		}
	} else {
		if(!stat %in% names(test)) {
			stop(paste0(stat, ' is not a valid IRR statistic.'))
		}
		p <- ggplot(test, aes_string(x = 'agreement', y = stat, color = 'k')) +
			geom_point(alpha = point.alpha) +
			xlim(c(0,1)) +
			ylim(c(-0.1,1)) +
			xlab('Percent Rater Agreement') + ylab(stat)
		if(title) {
			p <- p + ggtitle(paste0(stat, ' with ',
							 paste0(unique(test$nLevels), collapse = ', '),
						     ' Scoring Levels and ',
						     test[1,]$k_per_event, ' out of ',
						     test[1,]$k, ' Raters per Scoring Event'))
		}
	}

	if(length(unique(test$k)) > 1) {
		p <- p + scale_color_hue('n Raters')
	} else {
		p <- p + scale_color_grey('') + theme(legend.position="none")
	}

	if(method == 'loess') {
		p <- p + geom_smooth(method = 'loess')
	} else if(method == 'linear') {
		p <- p + geom_smooth(method = 'lm')
	} else if(method == 'quadratic') {
		p <- p + geom_smooth(method = 'lm', formula = y ~ I(x ^ 2) + x)
	}

	return(p)
}
