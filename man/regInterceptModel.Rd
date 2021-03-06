% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heuristics.R
\name{regInterceptModel}
\alias{regInterceptModel}
\title{Linear regression wrapper for hueristica}
\usage{
regInterceptModel(train_matrix, criterion_col, cols_to_fit)
}
\arguments{
\item{train_matrix}{A matrix (or data.frame) of data to train (fit) the
model with.}

\item{criterion_col}{The index of the criterion column-- "y" in the formula.}

\item{cols_to_fit}{A vector of column indexes to fit-- the "x's" in the
formula.}
}
\value{
An object of class regInterceptModel, which is a subclass of lm.
}
\description{
A wrapper to create a lm model just specifying columns, generating
a model formula for you.  This makes it easier to run automated comparisons
with other models in heuristica.
}
\details{
This version assumes you always want to include the intercept.
}
\seealso{
\code{\link{regModel}} for a version that excludes the intercept.

\code{\link{predict.lm}} for prediction.

\code{\link{predictPairProb}} for predicting between a pair of rows.
}

