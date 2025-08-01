#' Test whether two debiased IOp estimates are significantly different
#'
#' `IOptest` is a post-estimation function of IOp. By taking two IOp estimates
#' and their standard errors it tests whether the difference in IOp is significant.
#'
#' @param iop_a Debiased IOp estimate of group A
#' @param iop_b Debiased IOp estimate of group A
#' @param se_a Debiased IOp standard error of group A
#' @param se_b Debiased IOp standard error of group B
#' @returns difference estimate, standard error, tstat and pvalue
#' @examples
#' set.seed(123)
#' nA <- 3000
#' X1A <- rnorm(nA)
#' X2A <- rnorm(nA)
#' YA <- exp(2 + 0*X1A + 0.1*X2A + rnorm(nA,0,0.5))
#' XA <- data.frame(X1A,X2A)
#' nB = 3000
#' X1B <- rnorm(nB)
#' X2B <- rnorm(nB)
#' YB <- exp(2 + 0.2*X1B + 0.1*X2B + rnorm(nB,0,0.5))
#' XB <- data.frame(X1B,X2B)
#'
#' ML = "XGB"
#' iopA <- IOp(YA,XA,ML = ML,sterr = TRUE, IOp_rel = TRUE,
#'            est_method = "Debiased", fitted_values = TRUE, CFit = TRUE)
#' iopB <- IOp(YB,XB,ML = ML,sterr = TRUE, IOp_rel = TRUE,
#'            est_method = "Debiased", fitted_values = TRUE, CFit = TRUE)
#'
#' iop_a <- iopA$IOp["IOp","IOp"]
#' se_a <- iopA$IOp["se","Gini"]
#' iop_b <- iopB$IOp["IOp","Gini"]
#' se_b <- iopB$IOp["se","Gini"]
#'
#' IOptest(iop_a, iop_b, se_a, se_b)
#'
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#'
#' Terschuur, J. (2022). Debiased Machine Learning Inequality
#' of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
#' @export
IOptest <- function(iop_a, iop_b, se_a, se_b){
  th <- iop_a - iop_b
  se <- sqrt(se_a^2 + se_b^2)
  tstat <- th/se
  pval <- 2*(1 - pnorm(abs(tstat)))
  c(difference = th, se = se, t.stat = tstat, pvalue = pval)
}
