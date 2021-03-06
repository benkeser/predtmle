% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapper_functions.R
\name{xgboost_wrapper}
\alias{xgboost_wrapper}
\title{Wrapper for fitting eXtreme gradient boosting via \code{xgboost}}
\usage{
xgboost_wrapper(test, train, ntrees = 500, max_depth = 4,
  shrinkage = 0.1, minobspernode = 2, params = list(), nthread = 1,
  verbose = 0, save_period = NULL)
}
\arguments{
\item{test}{A list with named objects \code{Y} and \code{X} (see description).}

\item{train}{A list with named objects \code{Y} and \code{X} (see description).}

\item{ntrees}{See \link[xgboost]{xgboost}}

\item{max_depth}{See \link[xgboost]{xgboost}}

\item{shrinkage}{See \link[xgboost]{xgboost}}

\item{minobspernode}{See \link[xgboost]{xgboost}}

\item{params}{See \link[xgboost]{xgboost}}

\item{nthread}{See \link[xgboost]{xgboost}}

\item{verbose}{See \link[xgboost]{xgboost}}

\item{save_period}{See \link[xgboost]{xgboost}}
}
\value{
A list with named objects (see description).
}
\description{
Compatible learner wrappers for this package should have a specific format.
Namely they should take as input a list called \code{train} that contains
named objects \code{$Y} and \code{$X}, that contain, respectively, the outcomes
and predictors in a particular training fold. Other options may be passed in
to the function as well. The function must output a list with the following
named objects: \code{test_pred} = predictions of \code{test$Y} based on the learner
fit using \code{train$X}; \code{train_pred} = prediction of \code{train$Y} based 
on the learner fit using \code{train$X}; \code{model} = the fitted model (only 
necessary if you desire to look at this model later, not used for internal 
computations); \code{train_y} = a copy of \code{train$Y}; \code{test_y} = a copy
of \code{test$Y}.
}
\details{
This particular wrapper implements eXtreme gradient boosting using 
\link[xgboost]{xgboost}. We refer readers to the original package's 
documentation for more details.
}
\examples{
# simulate data
Q0 <- function(x){ plogis(x) }
# make list of training data
train_X <- data.frame(x1 = runif(50))
train_Y <- rbinom(50, 1, Q0(train_X$x1))
train <- list(Y = train_Y, X = train_X)
# make list of test data
test_X <- data.frame(x1 = runif(50))
test_Y <- rbinom(50, 1, Q0(train_X$x1))
test <- list(Y = test_Y, X = test_X)
# fit xgboost
xgb_wrap <- xgboost_wrapper(train = train, test = test)
}
