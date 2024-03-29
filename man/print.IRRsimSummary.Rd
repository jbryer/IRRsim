% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.IRRsim.R
\name{print.IRRsimSummary}
\alias{print.IRRsimSummary}
\title{Print the result of summary.IRRsim}
\usage{
\method{print}{IRRsimSummary}(x, ...)
}
\arguments{
\item{x}{the results from [IRRsim::summary.IRRsim()]}

\item{...}{currently unused.}
}
\value{
nothing returned.
}
\description{
The [IRRsim::summary.IRRsimm()] will return a list with \code{model} and
\code{summary} objects. The former are the results of the modeling function,
the latter is a \code{data.frame} that summarizes the predicted results of
the model. This function will only print the \code{data.frame}.
}
