% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppv_functions.R
\name{boot_scrnp}
\alias{boot_scrnp}
\title{Compute the bootstrap-corrected estimator of SCRNP.}
\usage{
boot_scrnp(Y, X, B = 200, learner = "glm_wrapper", sens = 0.95,
  correct632 = FALSE, ...)
}
\arguments{
\item{Y}{A numeric vector of outcomes, assume to equal \code{0} or \code{1}.}

\item{X}{A \code{data.frame} of variables for prediction.}

\item{B}{The number of bootstrap samples.}

\item{learner}{A wrapper that implements the desired method for building a 
prediction algorithm. See TODO: ADD DOCUMENTATION FOR WRITING}

\item{sens}{The sensitivity constraint to use.}

\item{correct632}{A boolean indicating whether to use the .632 correction.}

\item{...}{Other options, not currently used.}
}
\value{
A list with \code{$scrnp} the bootstrap-corrected estimate of SCRNP.
}
\description{
This estimator is computed by re-sampling with replacement (i.e., bootstrap
sampling) from the data. The SCRNP is computed for the learner trained on the 
full data. The SCRNP is then computed for the learner trained on each bootstrap
sample. The average difference between the full data-trained learner and 
the bootstrap-trained learner is computed to estimate the bias in the full-data-estimated
SCRNP. The final estimate of SCRNP is given by the difference in the full-data SCRNP 
and the estimated bias.
}
\examples{
# simulate data
X <- data.frame(x1 = rnorm(50))
Y <- rbinom(50, 1, plogis(X$x1))
# compute bootstrap estimate of scrnp for logistic regression
# use small B for fast run
boot <- boot_scrnp(Y = Y, X = X, B = 25, learner = "glm_wrapper")
}
