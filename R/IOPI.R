IOPI <- function(Y,
                 X,
                 ineq = c("Gini", "MLD",c("Gini","MLD")),
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
  iopi <- mliop(X,
                Y,
                ML = ML,
                OLSensemble = OLSensemble,
                SL.library = SL.library,
                ensemblefolds = ensemblefolds,
                ineq = ineq,
                IOp_rel = IOp_rel,
                fitted_values = fitted_values,
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
    if (fitted_values != TRUE){
      stop("For se with plug in set FVs = TRUE")
    }
      FVs <- iopi$FVs
      # se <- sepi(Y,FVs,iopi$IOp["IOp","Gini"], ineq = ineq, weights = weights)
      # se = se_deb(Y, FVs, as.matrix(iopi$IOp)[1,1], ineq = ineq, weights = weights)
      se = se_PI(Y, FVs, as.matrix(iopi$IOp)[1,1], weights = weights)
      se_naive = se$se_naive
      se = se$se
  }

  if (fitted_values == TRUE){
    FVs <- iopi$FVs
    iopi <- iopi$IOp
  }
  if (sterr == TRUE){
    if (IOp_rel == TRUE){
      se_rel = NA
      se_rel_naive = NA
      se = c(se,se_rel)
      se_naive = c(se_naive, se_rel_naive)
      warning("se for IOp rel plug in not coded")
      aux <- sapply(ineq,function(u){paste(u,"rel",sep = "_")})
      names(se) <- c(rbind(ineq,aux))
      iopi <- c(iopi)
      names(iopi) <- c(rbind(ineq,aux))
    } else{
      # names(se) <- ineq
      iopi <- c(iopi)
      names(iopi) <- ineq
    }
  } else{
    se <- NULL
    se_naive = NULL
    if (IOp_rel == TRUE){
      aux <- sapply(ineq,function(u){paste(u,"rel",sep = "_")})
      iopi <- c(iopi)
      names(iopi) <- c(rbind(ineq,aux))
    } else{
      iopi <- c(iopi)
      names(iopi) <- ineq
    }
  }
  if (IOp_rel == TRUE){
    IOp_res <- rbind(iopi,se,se_naive)
    IOp_rel_res <- rbind(iopi[paste(ineq,"rel",sep = "_")],
                         NA,NA)
    colnames(IOp_rel_res) <- ineq
    if (sterr == TRUE){
      rownames(IOp_res) <- c("IOp", "se", "se_naive")
      rownames(IOp_rel_res) <- c("IOp_rel", "se", "se_naive")
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
    IOp_res <- rbind(iopi[ineq],se,se_naive)
    if (sterr == TRUE){
      rownames(IOp_res) <- c("IOp", "se","se_naive")
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

