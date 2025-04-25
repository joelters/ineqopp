IOPI <- function(Y,
                 X,
                 ML = c("Lasso","Ridge","RF","CIF","XGB","CB",
                        "OLSensemble","SL"),
                 OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                 ensemblefolds = 2,
                 SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                 IOp_rel = FALSE,
                 sterr = FALSE,
                 fitted_values = TRUE,
                 weights = NULL,
                 rf.cf.ntree = 500,
                 rf.depth = NULL,
                 mtry = max(floor(ncol(X)/3), 1),
                 polynomial.Lasso = 1,
                 polynomial.Ridge = 1,
                 xgb.nrounds = 200,
                 xgb.max.depth = 6,
                 cb.iterations = 1000,
                 cb.depth = 6,
                 torch.epochs = 50,
                 torch.hidden_units = c(64, 32),
                 torch.lr = 0.01,
                 torch.dropout = 0.2,
                 extFVs = NULL){
  if(!is.null(weights) & class(weights) != "numeric"){
    stop("Weights have to be numeric")
  }
  iopi <- mliop_new2(X,
                Y,
                ML = ML,
                OLSensemble = OLSensemble,
                SL.library = SL.library,
                ensemblefolds = ensemblefolds,
                IOp_rel = IOp_rel,
                fitted_values = fitted_values,
                weights = weights,
                rf.cf.ntree = rf.cf.ntree,
                rf.depth = rf.depth,
                mtry = mtry,
                polynomial.Lasso = polynomial.Lasso,
                polynomial.Ridge = polynomial.Ridge,
                xgb.nrounds = xgb.nrounds,
                xgb.max.depth = xgb.max.depth,
                cb.iterations = cb.iterations,
                cb.depth = cb.depth,
                torch.epochs = torch.epochs,
                torch.hidden_units = torch.hidden_units,
                torch.lr = torch.lr,
                torch.dropout = torch.dropout,
                extFVs = extFVs)
  #Standard errors
  if(sterr == TRUE){
    if (fitted_values != TRUE){
      stop("For se with plug in set FVs = TRUE")
    }
    FVs <- iopi$FVs
    n = length(Y)
    alpha <- numeric(n)  # Preallocate memory
    for (u in 1:n) {
      alpha[u] <- (1 / (n-1)) * sum((FVs[u] > FVs[-u]) - (FVs[u] < FVs[-u]))
    }
    se = se_deb_new2(Y, FVs, alpha, as.matrix(iopi$IOp)[1,1], weights = weights)
  }

  if (fitted_values == TRUE){
    FVs <- iopi$FVs
    iopi <- iopi$IOp
  }
  if (sterr == TRUE){
    if (IOp_rel == TRUE){
      se_rel = NA
      se = c(se,se_rel)
      warning("se for IOp rel plug in not coded")
      names(se) <- c(rbind("Gini","Gini_rel"))
      iopi <- c(iopi)
      names(iopi) <- c(rbind("Gini","Gini_rel"))
    } else{
      names(se) <- "Gini"
      iopi <- c(iopi)
      names(iopi) <- "Gini"
    }
  } else{
    se <- NULL
    if (IOp_rel == TRUE){
      iopi <- c(iopi)
      names(iopi) <- c(rbind("Gini","Gini_rel"))
    } else{
      iopi <- c(iopi)
      names(iopi) <- "Gini"
    }
  }
  if (IOp_rel == TRUE){
    IOp_res <- rbind(iopi["Gini"],se["Gini"])
    IOp_rel_res <- rbind(iopi[paste("Gini","rel",sep = "_")],
                         se[paste("Gini","rel",sep = "_")])
    colnames(IOp_rel_res) <- "Gini"
    if (sterr == TRUE){
      rownames(IOp_res) <- c("IOp", "se")
      rownames(IOp_rel_res) <- c("IOp_rel", "se")
    } else{
      rownames(IOp_res) <- c("IOp")
      rownames(IOp_rel_res) <- c("IOp_rel")
    }
    if (fitted_values == TRUE){
      return(list(IOp = IOp_res, IOp_rel = IOp_rel_res, FVs = FVs))
    }
    else{return(list(IOp = IOp_res, IOp_rel = IOp_rel_res))}
  }
  else {
    IOp_res <- rbind(iopi["Gini"],se["Gini"])
    if (sterr == TRUE){
      rownames(IOp_res) <- c("IOp", "se")
    } else{
      rownames(IOp_res) <- c("IOp")
    }
    IOp_rel_res <- NULL
    if (fitted_values == TRUE){
      return(list(IOp = IOp_res, IOp_rel = IOp_rel_res, FVs = FVs))
    }
    else{return(list(IOp = IOp_res, IOp_rel = IOp_rel_res))}
  }
}

