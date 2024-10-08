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
#' @param ensemblefolds how many folds to use in crossvalidation for ensemble
#' methods (i.e. superlearner or OLSensemble)
#' @param sterr logical indicating whether standard errors should be computed
#' @param CFit logical indicating whether Cross-Fitting should be done in
#' the debiased estimators (no inferential guarantee can be given yet if FALSE)
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
#'
#' iop_pi <- IOp(Y,
#'               X,
#'               est_method = "Plugin",
#'               ineq = "Gini",
#'               ML = "RF",
#'               sterr = FALSE,
#'               IOp_rel = TRUE,
#'               fitted_values = TRUE)
#'
#' iop_deb <- IOp(Y,
#'                X,
#'                est_method = "Debiased",
#'                CFit = TRUE,
#'                ineq = "Gini",
#'                ML = "RF",
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
IOp_new <- function(Y,
                X,
                est_method = c("Plugin","Debiased"),
                ineq = c("Gini", "MLD",c("Gini", "MLD")),
                ML = c("Lasso","Ridge","RF","CIF","XGB","CB",
                      "OLSensemble", "SL"),
                OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                ensemblefolds = 5,
                sterr = TRUE,
                CFit = TRUE,
                IOp_rel = FALSE,
                fitted_values = FALSE,
                weights = NULL,
                rf.cf.ntree = 500,
                rf.depth = NULL,
                mtry = max(floor(ncol(X)/3), 1)){
  if (sum(Y<0) != 0){stop("There are negative values for Y.")}
  if (est_method == "Plugin"){
    io <- IOPI(Y,
               X,
               ineq = ineq,
               ML = ML,
               OLSensemble = OLSensemble,
               SL.library = SL.library,
               ensemblefolds = ensemblefolds,
               IOp_rel = IOp_rel,
               sterr = sterr,
               fitted_values = fitted_values,
               weights = weights,
               rf.cf.ntree = rf.cf.ntree,
               rf.depth = rf.depth,
               mtry = max(floor(ncol(X)/3), 1))
  }
  else if (est_method == "Debiased"){
    io <- IOD_new(Y,
              X,
              CFit = CFit,
              ineq = ineq,
              ML = ML,
              OLSensemble = OLSensemble,
              SL.library = SL.library,
              ensemblefolds = ensemblefolds,
              sterr = sterr,
              IOp_rel = IOp_rel,
              fitted_values = fitted_values,
              weights = weights,
              rf.cf.ntree = rf.cf.ntree,
              rf.depth = rf.depth,
              mtry = mtry)
  }
}
