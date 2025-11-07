#' Estimate Inequality of Opportunity (IOp)
#'
#' `IOp` estimates IOp by estimating the Gini of the fitted values in different ways. Plug in estimators are available
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
#' @param CFit logical indicating whether Cross-Fitting should be done in
#' the debiased estimators (no inferential guarantee can be given yet if FALSE)
#' @param IOp_rel logical indicating whether relative IOp should be computed
#' @param fitted_values a logical indicating whether (in sample) fitted values
#' should be computed. This can be useful for computing partial effects later.
#' @param weights survey weights adding up to 1
#' @param rf.cf.ntree how many trees should be grown when using RF or CIF
#' @param rf.depth how deep should trees be grown in RF (NULL is full depth,
#' NULL in ranger)
#' @param cf.depth how deep should trees be grown in CIF (Inf is full depth,
#' as in partykit)
#' @param polynomial.Lasso degree of polynomial to be fitted when using Lasso.
#' 1 just fits the input X. 2 squares all variables and adds
#' all pairwise interactions. 3 squares and cubes all variables and adds all
#' pairwise and threewise interactions...
#' @param polynomial.Ridge degree of polynomial to be fitted when using Ridge,
#' see polynomial.Lasso for more info.
#' @param polynomial.loglin degree of polynomial to be fitted when using loglin,
#' see polynomial.Lasso for more info.
#' @param mtry number of variables to consider at each split in RF or CIF
#' @param xgb.nrounds s an integer specifying how many rounds to use in XGB
#' @param xgb.max.depth an integer specifying how deep trees should be grown in XGB
#' @param cb.iterations an integer specifying how many iterations to use in CB
#' @param cb.depth an integer specifying how deep trees should be grown in CB
#' @param torch.epochs an integer specifying the number of epochs (full passes through the dataset)
#'  to use when training the Torch neural network.
#' @param torch.hidden_units a numeric vector specifying the number of neurons in
#'  each hidden layer of the Torch neural network.
#' @param torch.lr a numeric value specifying the learning rate to be used for the
#' optimizer when training the Torch neural network.
#' @param torch.dropout a numeric value between 0 and 1 specifying the dropout
#' rate for regularization in the Torch neural network.
#' @param polynomial.NLLS_ext degree of polynomial to be fitted when using
#'  NLLS_exp, see polynomial.Lasso for more info.
#'  @param start_nlls List with the starting values of the parameters.
#'  Default is log(mean(Y)) for the intercept and zero for all the rest.
#' @returns list containing IOp estimates, RMSE of the first stage (for Debiased
#' and DFit = TRUE, this are done with the triangles in the crossfitting), the fitted
#' values FVs (in Debiased with CFit = TRUE this FVs are in-sample not crossfitted,
#' the fitted values in each fold for the debiased crossfitted, the pairwise sign comparison
#' with the true signs (only if true fitted values FVs0 are known, e.g. in simulations) and
#' the coefficients of the OLSensemble if it is used.
#' @examples
#' X <- dplyr::select(mad2019,-Y)
#' Y <- mad2019$Y
#'
#'
#'
#' iop_pi <- IOp(Y,
#'               X,
#'               est_method = "Plugin",
#'               ML = "RF",
#'               sterr = FALSE,
#'               IOp_rel = TRUE,
#'               fitted_values = TRUE)
#'
#' iop_deb <- IOp(Y,
#'                X,
#'                est_method = "Debiased",
#'                CFit = TRUE,
#'                ML = "RF",
#'                sterr = TRUE,
#'                IOp_rel = TRUE,
#'                fitted_values = TRUE)
#'
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2023). Machine learning
#' inference on inequality of opportunity. arXiv preprint arXiv:2206.05235.
#'
#'
#'
#' @export
IOp <- function(Y,
                X,
                est_method = "Debiased",
                ML = c("Lasso","Ridge","RF","CIF","XGB","CB","Torch",
                      "loglin", "NLLS_exp", "OLSensemble", "SL"),
                OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                ensemblefolds = 5,
                sterr = TRUE,
                CFit = TRUE,
                IOp_rel = TRUE,
                fitted_values = FALSE,
                weights = NULL,
                rf.cf.ntree = 500,
                rf.depth = 5,
                cf.depth = 5,
                polynomial.Lasso = 1,
                polynomial.Ridge = 1,
                polynomial.loglin = 1,
                xgb.nrounds = 200,
                xgb.max.depth = 6,
                cb.iterations = 1000,
                cb.depth = 6,
                torch.epochs = 50,
                torch.hidden_units = c(64, 32),
                torch.lr = 0.01,
                torch.dropout = 0.2,
                mtry = max(floor(ncol(X)/3), 1),
                polynomial.NLLS_exp = 1,
                start_nlls = NULL,
                FVs0 = NULL,
                extFVs = NULL){
  if (sum(Y<0) != 0){warning("There are negative values for Y.")}
  if (est_method == "Plugin"){
    io <- IOPI(Y,
               X,
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
               cf.depth = cf.depth,
               polynomial.Lasso = polynomial.Lasso,
               polynomial.Ridge = polynomial.Ridge,
               polynomial.loglin = polynomial.loglin,
               mtry = mtry,
               xgb.nrounds = xgb.nrounds,
               xgb.max.depth = xgb.max.depth,
               cb.iterations = cb.iterations,
               cb.depth = cb.depth,
               torch.epochs = torch.epochs,
               torch.hidden_units = torch.hidden_units,
               torch.lr = torch.lr,
               torch.dropout = torch.dropout,
               polynomial.NLLS_exp = polynomial.NLLS_exp,
               start_nlls = start_nlls,
               extFVs = extFVs)
  }
  else if (est_method == "Debiased"){
    io <- IOD(Y,
              X,
              CFit = CFit,
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
              cf.depth = cf.depth,
              polynomial.Lasso = polynomial.Lasso,
              polynomial.Ridge = polynomial.Ridge,
              polynomial.loglin = polynomial.loglin,
              polynomial.NLLS_exp = polynomial.NLLS_exp,
              start_nlls = start_nlls,
              mtry = mtry,
              xgb.nrounds = xgb.nrounds,
              xgb.max.depth = xgb.max.depth,
              cb.iterations = cb.iterations,
              cb.depth = cb.depth,
              torch.epochs = torch.epochs,
              torch.hidden_units = torch.hidden_units,
              torch.lr = torch.lr,
              torch.dropout = torch.dropout,
              FVs0 = FVs0,
              extFVs = extFVs)
  }
}
