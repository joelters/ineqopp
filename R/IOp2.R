#' Estimate Inequality of Opportunity (IOp)
#'
#' `IOp` estimates IOp in different ways. Plug in estimators are available
#' where the fitted values are estimated
#' by any machine learner among Lasso, Ridge,
#' Random Forest, Conditional Inference Forest, Extreme Gradient
#' Boosting, Catboosting or a (cross-validated) optimal combination of any
#' of these using the SuperLearner package. Debiased estimates based on the aforementioned
#' machine learners can also be computed with this function. In this case the
#' (optional) standard errors are derived analytically and backed by inferential
#' theory. A measure of partial effects and its standard errors for the debiased
#' measures can also be computed.
#'
#' @param Y is a vector containing the (continuous) outcome of interest
#' @param X is a dataframe containing all the circumstances
#' @param ML is a string specifying which machine learner to use
#' @param ensemble is a string vector specifying which learners
#' should be used in the SuperLearner
#' @param sterr logical indicating whether standard errors should be computed
#' @param CFit logical indicating whether Cross-Fitting should be done in
#' the debiased estimators (no inferential guarantee can be given yet if FALSE)
#' @param npart in how many parts should the data be split for cross-fitting
#' @param IOp_rel logical indicating whether relative IOp should be computed
#' @param fitted_values a logical indicating whether (in sample) fitted values
#' should be computed. This can be useful for computing partial effects later.
#' @param weights survey weights adding up to 1
#' @returns list containing IOp estimates, RMSE of the first stage (for Debiased
#' estimates), relative IOp (if desired) and fitted values (if desired)
#' @examples
#' X <- dplyr::select(mad2019,-Y)
#' Y <- mad2019$Y
#'
#'
#' iop_pi <- IOp(Y,
#'               X,
#'               est_method = "Plugin",
#'               ineq = "Gini",
#'               ML = "SL",
#'               ensemble = c("SL.Lasso","SL.CB"),
#'               sterr = FALSE,
#'               IOp_rel = TRUE,
#'               fitted_values = TRUE)
#'
#' iop_deb <- IOp(Y,
#'                X,
#'                est_method = "Debiased",
#'                CFit = TRUE,
#'                ineq = "Gini",
#'                ML = "Lasso",
#'                sterr = TRUE,
#'                IOp_rel = TRUE,
#'                fitted_values = TRUE)
#'
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#'
#' Terschuur, J. (2022). Debiased Machine Learning Inequality
#' of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
#' @export
IOp2 <- function(Y,
                X,
                est_method = c("Plugin","Debiased"),
                ineq = c("Gini", "MLD",c("Gini", "MLD")),
                plugin_method = c("loglin", "NP", "ML"),
                ML = c("Lasso","Ridge","RF","CIF","XGB","CB","SL"),
                ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                sterr = TRUE,
                boots = 100,
                CFit = TRUE,
                npart = 5,
                IOp_rel = FALSE,
                fitted_values = FALSE,
                weights = NULL,
                verbose = FALSE){
  if (sum(Y<0) != 0){stop("There are negative values for Y.")}
  if (est_method == "Plugin"){
    io <- IOPI(Y,
               X,
               ineq = ineq,
               ML = ML,
               ensemble = ensemble,
               IOp_rel = IOp_rel,
               sterr = sterr,
               fitted_values = fitted_values,
               weights = weights)
  }
  else if (est_method == "Debiased"){
    io <- IOD2(Y,
              X,
              CFit = CFit,
              npart = npart,
              ineq = ineq,
              ML = ML,
              ensemble = ensemble,
              sterr = sterr,
              IOp_rel = IOp_rel,
              fitted_values = fitted_values,
              weights = weights,
              verbose = verbose)
  }
}
