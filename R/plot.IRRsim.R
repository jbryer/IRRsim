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
						...) {
	p <- NULL
	test <- as.data.frame(x)
	if(missing(stat)) {
		tests.melted <- reshape2::melt(test, id.vars = c('nLevels', 'nEvents', 'k', 'simAgreement', 'agreement'))
		p <- ggplot(tests.melted, aes(x = agreement, y = value)) +
			geom_point(alpha = point.alpha) +
			scale_color_hue('n Raters') +
			facet_wrap(~ variable) +
			xlim(c(0,1)) +
			xlab('Percent Rater Agreement') + ylab('ICC') +
			ggtitle(paste0('Inter-Rater Reliability with ', test[1,]$nLevels, ' Scoring Levels and ',
						   test[1,]$k, ' Raters'))
	} else {
		if(!stat %in% names(test)) {
			stop(paste0(stat, ' is not a valid IRR statistic.'))
		}
		p <- ggplot(test, aes_string(x = 'agreement', y = stat)) +
			geom_point(alpha = point.alpha) +
			xlim(c(0,1)) +
			xlab('Percent Rater Agreement') + ylab(stat) +
			ggtitle(paste0(stat, ' with ', test[1,]$nLevels, ' Scoring Levels and ',
						   test[1,]$k, ' Raters'))
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
