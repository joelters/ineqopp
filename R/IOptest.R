#' Test whether two debiased IOp estimates are significantly different
#'
#' `IOptest` is a post-estimation function of IOp. By taking two IOp estimates
#' and their standard errors it tests whether the difference in IOp is siginificant.
#'
#' @param iop_a Debiased IOp estimate of group A
#' @param iop_b Debiased IOp estimate of group A
#' @param se_a Debiased IOp standard error of group A
#' @param se_b Debiased IOp standard error of group B
#' @returns difference estimate, standard error, tstat and pvalue
#' @examples
#' A <- dplyr::filter(mad2019, sex == "Female")
#' B <- dplyr::filter(mad2019, sex == "Male")
#'
#' XA <- dplyr::select(A,-Y)
#' YA <- A$Y
#' XB <- dplyr::select(B,-Y)
#' YB <- B$Y
#' iopA <- IOp(YA,
#'             XA,
#'             est_method = "Debiased",
#'             CFit = TRUE,
#'             ineq = "Gini",
#'             plugin_method = c("ML"),
#'             ML = "XGB",
#'             sterr = TRUE)
#' iopB <- IOp(YB,
#'             XB,
#'             est_method = "Debiased",
#'             CFit = TRUE,
#'             ineq = "Gini",
#'             plugin_method = c("ML"),
#'             ML = "XGB",
#'             sterr = TRUE)
#'
#' iop_a <- iopA$IOp["IOp","Gini"]
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
  pval <- 2*(pnorm(1 - abs(tstat)))
  c(difference = th, se = se, t.stat = tstat, pvalue = pval)
}
