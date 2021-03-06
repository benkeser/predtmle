% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auc_functions.R
\name{cv_auc}
\alias{cv_auc}
\title{Cross-validated area under the receiver operating characteristics curve (AUC)}
\usage{
cv_auc(Y, X, K = 10, learner = "glm_wrapper", nested_cv = TRUE,
  nested_K = K - 1, parallel = FALSE, max_cvtmle_iter = 10,
  cvtmle_ictol = 1/length(Y), prediction_list = NULL, ...)
}
\arguments{
\item{Y}{A numeric vector of outcomes, assume to equal \code{0} or \code{1}.}

\item{X}{A \code{data.frame} or \code{matrix} of variables for prediction.}

\item{K}{The number of cross-validation folds (default is \code{10}).}

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
\code{cvtmle_ictol}.}

\item{prediction_list}{For power users: a list of predictions made by \code{learner}
that has a format compatible with \code{cvauc}.}

\item{...}{Other arguments, not currently used}
}
\value{
A list TO DO: More documentation here
}
\description{
This function computes K-fold cross-validated estimates of the area under
the receiver operating characteristics (ROC) curve (hereafter, AUC). This
quantity can be interpreted as the probability that a randomly selected 
case will have higher predicted risk than a randomly selected control.
}
\details{
To estimate the AUC of a particular prediction algorithm, K-fold cross-validation
is commonly used. The data are partitioned into K distinct groups. The 
prediction algorithm is developed using K-1 of these groups. In standard K-fold
cross-validation, the AUC of this prediction algorithm is estimated using
the remaining fold
}
\examples{
n <- 200
p <- 10
X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
Y <- rbinom(n, 1, plogis(X[,1] + X[,10]))
fit <- cv_auc(Y = Y, X = X, K = 5, learner = "glm_wrapper")
}
