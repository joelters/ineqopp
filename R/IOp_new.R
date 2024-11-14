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
#' @param est_method is a string specifying which estimation method to use
#' (Plugin or Debiased)
#' @param ineq is a string specifying which inequality measure to use
#' (Gini or MLD) or a vector of both
#' @param ML is a string specifying which machine learner to use
#' @param OLSensemble is a string vector specifying which learners should be
#' used in OLS ensemble method
#' @param SL.library is a string vector specifying which learners should be
#' used in SuperLearner
#' @param ensemblefolds how many folds to use in crossvalidation for ensemble
#' methods (i.e. superlearner or OLSensemble)
#' @param sterr logical indicating whether standard errors should be computed
#' @param sterr_type integers 1 or 2, default 1. Specifies which type of standard
#' errors should be. sterr_type 1 computes only first order variance
#' (hayek projection variance) and sterr_type 2 is an unbiased variance
#' estimator of all orders (sigma_u in Iwashita and Klar (2024)).
#' @param CFit logical indicating whether Cross-Fitting should be done in
#' the debiased estimators (no inferential guarantee can be given yet if FALSE)
#' @param IOp_rel logical indicating whether relative IOp should be computed
#' @param fitted_values a logical indicating whether (in sample) fitted values
#' should be computed. This can be useful for computing partial effects later.
#' @param weights survey weights adding up to 1
#' @param rf.cf.ntree how many trees should be grown when using RF or CIF
#' @param rf.depth how deep should trees be grown in RF (NULL is default from ranger)
#' @param polynomial.Lasso degree of polynomial to be fitted when using Lasso.
#' 1 just fits the input X. 2 squares all variables and adds
#' all pairwise interactions. 3 squares and cubes all variables and adds all
#' pairwise and threewise interactions...
#' @param polynomial.Ridge degree of polynomial to be fitted when using Ridge,
#' see polynomial.Lasso for more info.
#' @param mtry number of variables to consider at each split in RF or CIF
#' @param xgb.nrounds s an integer specifying how many rounds to use in XGB
#' @param xgb.max.depth an integer specifying how deep trees should be grown in XGB
#' @param cb.iterations an integer specifying how many iterations to use in CB
#' @param cb.depth an integer specifying how deep trees should be grown in CB
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
#'
#' Iwashita, T., & Klar, B. (2024). A gamma tail statistic and its asymptotics.
#' Statistica Neerlandica, 78(2), 264-280.
#'
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
                sterr_type = 1,
                CFit = TRUE,
                IOp_rel = FALSE,
                fitted_values = FALSE,
                weights = NULL,
                rf.cf.ntree = 500,
                rf.depth = NULL,
                polynomial.Lasso = 1,
                polynomial.Ridge = 1,
                xgb.nrounds = 200,
                xgb.max.depth = 6,
                cb.iterations = 1000,
                cb.depth = 6,
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
               polynomial.Lasso = polynomial.Lasso,
               polynomial.Ridge = polynomial.Ridge,
               mtry = max(floor(ncol(X)/3), 1),
               xgb.nrounds = xgb.nrounds,
               xgb.max.depth = xgb.max.depth,
               cb.iterations = cb.iterations,
               cb.depth = cb.depth)
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
              sterr_type = sterr_type,
              IOp_rel = IOp_rel,
              fitted_values = fitted_values,
              weights = weights,
              rf.cf.ntree = rf.cf.ntree,
              rf.depth = rf.depth,
              polynomial.Lasso = polynomial.Lasso,
              polynomial.Ridge = polynomial.Ridge,
              mtry = mtry,
              xgb.nrounds = xgb.nrounds,
              xgb.max.depth = xgb.max.depth,
              cb.iterations = cb.iterations,
              cb.depth = cb.depth)
  }
}
