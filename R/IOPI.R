IOPI <- function(Y,
                 X,
                 ML = c("Lasso","Ridge","RF","CIF","XGB","CB",
                        "loglin", "NLLS_exp", "OLSensemble","SL"),
                 OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                 ensemblefolds = 2,
                 SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                 IOp_rel = FALSE,
                 sterr = FALSE,
                 fitted_values = TRUE,
                 weights = NULL,
                 rf.cf.ntree = 500,
                 rf.depth = NULL,
                 cf.depth = Inf,
                 mtry = max(floor(ncol(X)/3), 1),
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
                 polynomial.NLLS_exp = 1,
                 start_nlls = NULL,
                 extFVs = NULL){
  if(!is.null(weights) & class(weights) != "numeric"){
    stop("Weights have to be numeric")
  }
  iopi <- mliop_pi(X,
                Y,
                ML = ML,
                OLSensemble = OLSensemble,
                SL.library = SL.library,
                ensemblefolds = ensemblefolds,
                IOp_rel = IOp_rel,
                fitted_values = TRUE,
                weights = weights,
                rf.cf.ntree = rf.cf.ntree,
                rf.depth = rf.depth,
                cf.depth = cf.depth,
                mtry = mtry,
                polynomial.Lasso = polynomial.Lasso,
                polynomial.Ridge = polynomial.Ridge,
                polynomial.loglin = polynomial.loglin,
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
    #Standard errors
  if(sterr == TRUE){
    warning("Neither se nor se_naive are valid standard errors in general settings
    for the plug-in estimator. se uses the standard error formula of the debiased
    estimator and se_naive ignores first step estimation. Still neither are
    generally valid for the plugin estimator so we do not recommend reporting these.")
    FVs <- iopi$FVs
    rmse1 = sqrt(weighted.mean2(((Y - FVs)^2), weights = weights))
    iopi = iopi$IOp
    se = se_PI(Y, FVs, iopi["IOp"] , weights = weights)
    se_naive = se$se_naive
    se = se$se
    if (IOp_rel == TRUE){
      warning("se for IOp rel plug in not coded, NA instead")
      se = c(se,NA)
      se_naive = c(se_naive, NA)
      IOp_res = rbind(iopi,se,se_naive)
      return(list(IOp = IOp_res, RMSE1 = rmse1, FVs = FVs))
    } else{
      IOp_res = rbind(iopi,se)
      return(list(IOp = IOp_res,RMSE1 = rmse1, FVs = FVs))
    }
  } else{
    FVs <- iopi$FVs
    rmse1 = sqrt(weighted.mean2(((Y - FVs)^2), weights = weights))
    iopi = iopi$IOp
    return(list(IOp = iopi, RMSE1 = rmse1, FVs = FVs))
  }
}

