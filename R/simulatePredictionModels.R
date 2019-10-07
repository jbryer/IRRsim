#' Simulation for estimating logistic regression with uncertain responses.
#'
#' @param nSamples number of random datasets to simulate.
#' @param ... other parameters passed to \code{\link{simulatePredictionData}}.
#' @return a data frame with \code{nSamples} rows and columns with the
#'         predictive model accuracy.
#' @export
simulatePredictionModels <- function(nSamples = 500, ...) {
	df <- data.frame(row.names = 1:nSamples,
					 accuracy.full = rep(NA_real_, nSamples),
					 accuracy.known = rep(NA_real_, nSamples),
					 aic.full = rep(NA_real_, nSamples),
					 aic.known = rep(NA_real_, nSamples)
	)
	pb <- txtProgressBar(max = nSamples, style = 3)
	for(i in seq_len(nSamples)) { # TODO: make use multiple threads
		train <- simulatePredictionData(...)
		valid <- simulatePredictionData(...)
		# TODO: May want to include other prediction models
		uncertain_level <- attr(train, 'labels')[3]
		lr.full <- glm(Y ~ X,
					   data = train,
					   family = binomial(link = 'logit'))
		lr.known <- glm(Y ~ X,
						data = train[train$Group != uncertain_level,],
						family = binomial(link='logit'))
		valid$Fitted.Full <- predict(lr.full, newdata = valid)
		valid$Fitted.Known <- predict(lr.known, newdata = valid)
		valid$Predict.Full <- valid$Fitted.Full > 0.5
		valid$Predict.Known <- valid$Fitted.Known > 0.5
		tab.full <- table(valid$Y, valid$Fitted.Full > 0.5)
		tab.known <- table(valid$Y, valid$Fitted.Known > 0.5)
		df[i,]$accuracy.full <- nrow(valid[valid$Predict.Full == valid$Y,]) /
			nrow(valid)
		df[i,]$accuracy.known <- nrow(valid[valid$Predict.Known == valid$Y,]) /
			nrow(valid)
		df[i,]$aic.full <- lr.full$aic
		df[i,]$aic.known <- lr.known$aic
		setTxtProgressBar(pb, i)
	}
	df$accuracy.difference <- df$accuracy.full - df$accuracy.known
	close(pb)
	return(df)
}
