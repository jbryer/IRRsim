#' Simulate Inter-Rater Reliability Statistics
#'
#' @name IRRsim-package
#' @docType package
#' @title Simulate Inter-Rater Reliability Statistics
#' @author \email{jason@@bryer.org}
#' @keywords package IRR simulation interrater reliability
#' @import ggplot2
#' @import reshape2
#' @importFrom psych ICC cohen.kappa skew kurtosi
#' @importFrom foreach foreach
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster clusterEvalQ stopCluster
#' @importFrom doSNOW registerDoSNOW
NA

#' Simulated data used to create tables of expected IRR metrics
#'
#' This data frame is the results of simulating data sets with following parameters:
#' * Between 2 and 5 scoring levels
#' * Between 2 adn 12 raters
#' * Four response distributions including: uniform, lightly skewed, moderately
#'   skewed, and highly skewed.
#'
#' @name IRRsimData
#' @docType data
#' @usage data(IRRsimData)
#' @format A data.frame
#' @keywords datasets
'IRRsimData'

#' Guidelines for interpreting inter-rater reliability statistics.
#'
#' @name IRRguidelines
#' @docType data
#' @usage data(IRRguidelines)
#' @format A List object with numeric vectors containing the thresholds for
#'         interpreting IRR statistics. The names of the vectors correspond to
#'         the labels use by the authors. The name in the List object corresponds
#'         the first author's name. See vignette for more information.
#' @keywords datasets IRR ICC
#' @examples
#' data(IRRguidelines)
#' names(IRRguidelines) # Names of the guidelines
#' IRRguidelines[['Cicchetti']] # Cicchetti's (1994) guidelines
'IRRguidelines'
