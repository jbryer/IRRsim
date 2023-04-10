utils::globalVariables(c("value", "k", "X", "Group", "Y", "params"))


#' Simulate Inter-Rater Reliability Statistics
#'
#' @name IRRsim-package
#' @docType package
#' @title Simulate Inter-Rater Reliability Statistics
#' @author \email{jason@@bryer.org}
#' @keywords package IRR simulation interrater reliability
#' @import ggplot2
#' @import reshape2
#' @import shiny
#' @importFrom cowplot plot_grid
#' @importFrom psych ICC cohen.kappa skew kurtosi
#' @importFrom foreach foreach `%dopar%`
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster clusterEvalQ stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom stats lm as.formula loess predict runif glm binomial pnorm rnorm var
#' @importFrom utils getTxtProgressBar txtProgressBar setTxtProgressBar
NA

# THIS DATASET IS TOO LARGE TO INCLUDE IN THE PACKAGE
#
# Simulated data used to create tables of expected IRR metrics
#
# This data frame is the results of simulating data sets with following parameters:
# * Between 2 and 5 scoring levels
# * Between 2 adn 12 raters
# * Four response distributions including: uniform, lightly skewed, moderately
#   skewed, and highly skewed.
#
# @name IRRsimData
# @docType data
# @usage data(IRRsimData)
# @format A data.frame
# @keywords datasets
# 'IRRsimData'

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

#' Automated Student Assessment Prize Dataset 2
#'
#' Data from the Kaggle Automated Essay Scoring (AES) competition. This is essay
#' set 2 which used the following prompt:
#'
#' \emph{"All of us can think of a book that we hope none of our children or any other
#' children have taken off the shelf. But if I have the right to remove that
#' book from the shelf -- that work I abhor -- then you also have exactly the
#' same right and so does everyone else. And then we have no books left on the
#' shelf for any of us." --Katherine Paterson, Author}
#'
#' \emph{Write a persuasive essay to a newspaper reflecting your vies on censorship in
#' libraries. Do you believe that certain materials, such as books, music,
#' movies, magazines, etc., should be removed from the shelves if they are
#' found offensive? Support your position with convincing arguments from your
#' own experience, observations, and/or reading.}
#'
#' The data frame has the following variables:
#'
#' \describe{
#'     \item{essay_id}{A unique identifier for each individual student essay}
#'     \item{rater1_domain1}{Rater 1's domain 1 score}
#'     \item{rater2_domain1}{Rater 2's domain 1 score}
#'     \item{domain1_score}{Resolved score between the raters}
#'     \item{rater1_domain2}{Rater 1's domain 2 score}
#'     \item{rater2_domain2}{Rater 2's domain 2 score}
#'     \item{domain2_score}{Resolved score between the raters}
#'     \item{essay}{The ascii text of a student's response}
#' }
#'
#' More information available here: \url{https://www.kaggle.com/c/asap-aes/data}
#'
#' @examples
#' data(asap2)
'asap2'
