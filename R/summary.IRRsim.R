#' Returns a summary of a IRR simulation.
#'
#' This function will provide predicted IRR statistics for various percent
#' agreements. There are two elements in the returned list: \code{model} and
#' \code{summary}. The former are the results of the modeling function and the
#' latter is a \code{data.frame} providing the predicted values for various
#' percent agreements. If \code{stat = 'all'} is specified, the summary
#' table contains the predicted values. If a specific IRR statistic is specified,
#' the summary table contains the predicted value along with the interval.#'
#'
#' @param x the result of \code{\link{simulateICC}}.
#' @param method the method used to find predicted values. Possible values are
#'        loess, linear, and quadratic.
#' @param agreements vector of percent agreements to include in the summary table.
#' @param stat the IRR statistic to return summary for, or missing for all metrics.
#' @param k which number of raters to print summary of.
#' @param ... currently unused.
#' @export
summary.IRRsim <- function(object,
						   method = 'loess',
						   agreements = seq(0.10, 0.90, by = 0.1),
						   stat,
						   k,
						   predict.interval = "confidence",
						   ...) {
	prediction.df <- NULL
	model.out <- NULL

	test <- as.data.frame(object)
	raters <- unique(test$k)
	if(missing(k) & length(raters) > 1) {
		k <- list()
		model.out <- list()
		prediction.df <- list()
		for(j in 1:length(raters)) {
			tmp <- summary.IRRsim(object,
								  method = method,
								  agreements = agreements,
								  stat = stat,
								  k = raters[j])
			k[[j]] <- raters[j]
			model.out[[j]] <- tmp$model
			prediction.df[[j]] <- tmp$summary
		}
	} else {
		if(missing(k)) {
			k <- raters[1]
		}
		test <- test[test$k == k,]

		if(missing(stat)) {
			model.out <- list()
			prediction.df <- data.frame(Agreement = agreements)
			df <- as.data.frame(object)
			irr.stats <- names(df)[6:ncol(df)]
			for(i in irr.stats) {
				tmp <- summary.IRRsim(object,
									  method = method,
									  agreements = agreements,
									  stat = i,
									  k = k)
				model.out[[i]] <- tmp$model
				prediction.df[,i] <- tmp$summary[,i]
			}
		} else {
			if(!stat %in% names(test)) {
				stop(paste0("'", stat, "' is not a valid IRR statistic."))
			}
			if(method == 'loess') {
				formu <- as.formula(paste0(stat, ' ~ agreement'))
				model.out <- loess(formu, data = test)
				predict.out <- predict(model.out, newdata = agreements,
									   se = TRUE, interval = predict.interval)
				prediction.df <- data.frame(Agreement = agreements,
											IRR = predict.out$fit,
											Low = predict.out$fit - 1.96 * predict.out$se.fit,
											High = predict.out$fit + 1.96 * predict.out$se.fit,
											stringsAsFactors = FALSE)
			} else if(method == 'quadratic') {
				model.out <- lm(as.formula(paste0(stat, ' ~ I(agreement^2) + agreement')),
								data = test)
				prediction.out <- predict(model.out,
										  newdata = data.frame(agreement = agreements),
										  interval = predict.interval)
				prediction.df <- data.frame(Agreement = agreements,
											IRR = prediction.out[,'fit'],
											Low = prediction.out[,'lwr'],
											High = prediction.out[,'upr'])

			} else if(method == 'linear') {
				model.out <- lm(as.formula(paste0(stat, ' ~ agreement')), data = test)
				prediction.out <- predict(model.out,
										  newdata = data.frame(agreement = agreements),
										  interval = predict.interval)
				prediction.df <- data.frame(Agreement = agreements,
											IRR = prediction.out[,'fit'],
											Low = prediction.out[,'lwr'],
											High = prediction.out[,'upr'])
			} else {
				stop(paste0('Unsupported method specified: ', method))
			}
			names(prediction.df)[2:4] <- c(stat, paste0(stat, '.Low'),
										   paste0(stat, '.High'))
		}
	}

	result <- list(k = k, model = model.out, summary = prediction.df, data = test)
	class(result) <- c('IRRsimSummary', 'list')
	return(result)
}

#' Print the result of summary.IRRsim
#'
#' The \code{\link{summary.IRRsimm}} will return a list with \code{model} and
#' \code{summary} objects. The former are the results of the modeling function,
#' the latter is a \code{data.frame} that summarizes the predicted results of
#' the model. This function will only print the \code{data.frame}.
#'
#' @param x the results from \code{\link{summary.IRRsim}}
#' @param ... currently unused.
#' @export
print.IRRsimSummary <- function(x, ...) {
	if(length(x$k) > 1) {
		for(i in 1:length(x$k)) {
			cat(paste0("Prediction table for ", x$k[[i]], " raters.\n"))
			print(x$summary[[i]])
			cat("\n")
		}
	} else {
		cat(paste0("Prediction table for ", x$k, " raters.\n"))
		print(x$summary)
		cat("\n")
	}
}
