% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IOptest.R
\name{IOptest}
\alias{IOptest}
\title{Test whether two debiased IOp estimates are significantly different}
\usage{
IOptest(iop_a, iop_b, se_a, se_b)
}
\arguments{
\item{iop_a}{Debiased IOp estimate of group A}

\item{iop_b}{Debiased IOp estimate of group A}

\item{se_a}{Debiased IOp standard error of group A}

\item{se_b}{Debiased IOp standard error of group B}
}
\value{
difference estimate, standard error, tstat and pvalue
}
\description{
\code{IOptest} is a post-estimation function of IOp. By taking two IOp estimates
and their standard errors it tests whether the difference in IOp is significant.
}
\examples{
set.seed(123)
nA <- 3000
X1A <- rnorm(nA)
X2A <- rnorm(nA)
YA <- exp(2 + 0*X1A + 0.1*X2A + rnorm(nA,0,0.5))
XA <- data.frame(X1A,X2A)
nB = 3000
X1B <- rnorm(nB)
X2B <- rnorm(nB)
YB <- exp(2 + 0.2*X1B + 0.1*X2B + rnorm(nB,0,0.5))
XB <- data.frame(X1B,X2B)

ML = "XGB"
iopA <- IOp(YA,XA,ML = ML,sterr = TRUE, IOp_rel = TRUE,
           est_method = "Debiased", fitted_values = TRUE, CFit = TRUE)
iopB <- IOp(YB,XB,ML = ML,sterr = TRUE, IOp_rel = TRUE,
           est_method = "Debiased", fitted_values = TRUE, CFit = TRUE)

iop_a <- iopA$IOp["IOp","IOp"]
se_a <- iopA$IOp["se","Gini"]
iop_b <- iopB$IOp["IOp","Gini"]
se_b <- iopB$IOp["se","Gini"]

IOptest(iop_a, iop_b, se_a, se_b)


}
\references{
Escanciano, J. C., & Terschuur, J. R. (2022).
Debiased Semiparametric U-Statistics: Machine Learning Inference
on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.

Terschuur, J. (2022). Debiased Machine Learning Inequality
of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
}
