% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppv_functions.R
\name{cv_scrnp}
\alias{cv_scrnp}
\title{Compute CVTML estimates of cross-validated AUC}
\usage{
cv_scrnp(Y, X, K = 10, sens = 0.95, learner = "glm_wrapper",
  nested_cv = TRUE, nested_K = K - 1, parallel = FALSE,
  max_cvtmle_iter = 10, cvtmle_ictol = 1/length(Y),
  quantile_type = 8, prediction_list = NULL, ...)
}
\arguments{
\item{Y}{A numeric vector of outcomes, assume to equal \code{0} or \code{1}.}

\item{X}{A \code{data.frame} or \code{matrix} of variables for prediction.}

\item{K}{The number of cross-validation folds (default is \code{10}).}

\item{sens}{The sensitivity constraint imposed on the rate of negative prediction
(see description).}

\item{learner}{A wrapper that implements the desired method for building a 
prediction algorithm. See TODO: ADD DOCUMENTATION FOR WRITING}

\item{nested_cv}{A boolean indicating whether nested cross validation should
be used to estimate the distribution of the prediction function. Default (\code{TRUE})
is best choice for aggressive \code{learner}'s, while \code{FALSE} is reasonable
for smooth \code{learner}'s (e.g., logistic regression).}

\item{nested_K}{If nested cross validation is used, how many inner folds should 
there be? Default (\code{K-1}) affords quicker computation by reusing training
fold learner fits.}

\item{parallel}{A boolean indicating whether prediction algorithms should be 
trained in parallel. Default to \code{FALSE}.}

\item{max_cvtmle_iter}{Maximum number of iterations for the bias correction
step of the CV-TMLE estimator (default \code{10}).}

\item{cvtmle_ictol}{The CV-TMLE will iterate \code{max_cvtmle_iter} is reached 
or mean of cross-validated efficient influence function is less than 
\code{cvtmle_cvtmle_ictol}.}

\item{quantile_type}{Type of quantile estimator to be used. See \link[stats]{quantile}
for description.}

\item{prediction_list}{For power users: a list of predictions made by \code{learner}
that has a format compatible with \code{cvauc}.}

\item{...}{Other arguments, not currently used}
}
\value{
A list TO DO: more documentation here.
}
\description{
TO DO: Add description
}
\examples{
n <- 200
p <- 10
X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
Y <- rbinom(n, 1, plogis(X[,1] + X[,10]))
fit <- cv_scrnp(Y = Y, X = X, K = 5, nested_cv = FALSE, learner = "glm_wrapper")

}
