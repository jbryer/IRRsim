#' Logistic Regression Plot
#'
#' @param x the results of \code{\link{simulatePredictionData}}.
#' @param colors the color scheme used for the three groups.
#' @param rel_heights the relative heights of histogram to scatter plot.
#' @param ... other parameters passed to \code{cowplot::plot_grid}.
#' @param a ggplot2 plot of the logistic regerssion results.
#' @export
plot.lrdf <- function(x,
					  colors = c("#1b9e77","#7570b3","#d95f02"),
					  rel_heights = c(.4, .6),
					  title,
					  ...) {
	plot.dist <- ggplot(x, aes(x = X, color = Group)) +
		geom_density() +
		scale_color_manual(values = colors) +
		theme(legend.position = 'none',
			  plot.title = element_text(hjust=0),
			  plot.subtitle = element_text(hjust=0)) +
		xlab('') + ylab('Density')

	if(missing(title)) {
		plot.dist <- plot.dist + ggtitle(paste0('Logistic Regression Results'),
					subtitle = bquote(atop(mu[A] == .(attr(x, 'mean1')) ~ ";" ~
									  n[A] == .(attr(x, 'n1')) ~ ';' ~
									  mu[B] == .(attr(x, 'mean2')) ~ ';' ~
									  n[B] == .(attr(x, 'n2')) ~ ';' ~
									  SD == .(attr(x, 'sd')) ,
									  mu[Unknown] == .(attr(x, 'mean_unknown')) ~ ';' ~
									  n[Unknown] == .(attr(x, 'n_uknown')) ~ ';' ~
									  Ratio[A:B] == .(attr(x, 'ratio_unknown')) ~
									  phantom (1000000)
									  )	))
	} else {
		plot.dist <- plot.dist + ggtitle(title)
	}

	uncertain_level <- attr(x, 'labels')[3]
	data2 <- x[x$Group != uncertain_level,]
	plot.lr <- ggplot(x, aes(x = X, y = Y)) +
		geom_point(aes(color = Group), alpha = 0.4) +
		geom_smooth(method = "glm",
					method.args = list(family = "binomial"),
					se = FALSE,
					color = colors[3]) +
		geom_smooth(data = data2,
					method = "glm",
					method.args = list(family = "binomial"),
					se = FALSE,
					color = 'black') +
		scale_color_manual(values = colors) +
		theme(legend.position = 'none')

	cowplot::plot_grid(plot.dist, plot.lr,
			  rel_heights = rel_heights, ncol = 1)
}
