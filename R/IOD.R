IOD <- function(Y,
                X,
                CFit = TRUE,
                ML = c("Lasso","Ridge","RF","CIF","XGB","CB", "Torch",
                      "loglin", "NLLS_exp", "OLSensemble", "SL"),
                OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                ensemblefolds = 5,
                sterr = TRUE,
                weights = NULL,
                IOp_rel = FALSE,
                fitted_values = FALSE,
                rf.cf.ntree = 500,
                rf.depth = NULL,
                cf.depth = Inf,
                polynomial.Lasso = 1,
                polynomial.Ridge = 1,
                polynomial.loglin = 1,
                mtry = max(floor(ncol(X)/3), 1),
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
                FVs0 = NULL,
                extFVs = NULL){
  if(!is.null(weights) & class(weights) != "numeric"){
    stop("Weights have to be numeric")
  }
  #method takes the value of RF_type
  ML = match.arg(ML)
  # ineq = match.arg(ineq)
  if (CFit == FALSE){
    #Model and FVs estimation
    if(is.null(extFVs)){
      m <- ML::MLest(X,
                   Y,
                   ML,
                   OLSensemble = OLSensemble,
                   SL.library = SL.library,
                   ensemblefolds = ensemblefolds,
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
                   FVs = TRUE,
                   weights = weights)
    model <- m$model
    FVs <- m$FVs
    }
    if(!is.null(extFVs)){
      FVs = extFVs
    }

    if(sum(FVs <= 0) != 0){
      warning(paste(sum(FVs <= 0),"FVs were lower or equal than 0 and were
                    turned into the value 1."))
    }

    #Debiased IOp
    iodeb <- io_deb(Y, FVs, weights = weights)

    if (sterr == TRUE){
      warning("Standard errors without cross-fitting are not valid in general when
              using ML methods.")
      se <- se_deb(Y, FVs, iodeb, weights = weights)
    } else{se <- NULL}
    if (IOp_rel == TRUE){
      G <- acid::weighted.gini(Y,weights)
      G <- as.numeric(G[2])
      iorel <- iodeb/G
      if(sterr == TRUE){
        se_rel <- se_rel(Y,FVs,iodeb, IY = G, weights = weights)
      } else{
        se_rel = NULL
      }
    }
    resiop <- c(iodeb,se)
    names(resiop) <- c("IOp", "se")
    if (IOp_rel == TRUE){
      res_rel <- c(iorel,se_rel)
      names(res_rel) <- c("IOp_rel", "se")
    } else{
      res_rel <- NULL
    }
    rmse1 = sqrt(weighted.mean2(((Y - FVs)^2), weights = weights))
    resiop = cbind(resiop,res_rel)
    colnames(resiop) = c("IOp","IOp_rel")
    return(list(IOp = resiop, RMSE1 = rmse1, FVs = FVs))
  }
  else{
    nn <- length(Y)
    if (is.null(FVs0)){
      FVs0 = rep(NA,nn)
    }
    weights <- if (is.null(weights)) rep(1/nn,nn) else weights
    df <- as.data.frame(cbind(cbind(Y = Y, wt = weights, FVs0 = FVs0),X))
    names(df)[1] = "Y"
    # reshufle data in case there is some order
    df = df[sample(1:nn),]
    rownames(df) = paste(1:nn)
    dfcf <- SP(df)
    dfcfi <- dfcf$dfcfi
    dfcfj <- dfcf$dfcfj

    numcf <- 0
    res = lapply(1:6, function(u){
      message("Fold ", u, " of ", 6)
      #Create dataframe with all observations not in I_l
      aux <- dfnotl(df,dfcfi,dfcfj,u)

      #Train model with observations not in I_l (not in K = i or K = j)
      m <- ML::MLest(dplyr::select(aux,-c(Y,wt,FVs0)),
                     aux$Y,
                     ML,
                     OLSensemble = OLSensemble,
                     SL.library = SL.library,
                     ensemblefolds = ensemblefolds,
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
                     FVs = FALSE,
                     weights = aux$wt)
      if (ML != "OLSensemble"){
        model <- m$model
        coefs = NULL
      } else{
        coefs = m$coefs
        model = m$model
      }

      #Predict fitted values for obs in Ci and Cj
      #using model trained with obs not in Ci or Cj
      #Y1 are income observations in Ci, FVs1 are
      #predictions on Ci with model trained with obs
      #not in Ci or Cj
      #Y2 are income observations in Cj, FVs2 are
      #predictions on Cj with model trained with obs
      #not in Ci or Cj

      X1 <- dplyr::select(dfcfi[[u]][,-c(ncol(dfcfi[[u]]))], -c(Y,wt,FVs0))
      Y1 <- dfcfi[[u]]$Y
      FVs01 <- dfcfi[[u]]$FVs0
      FVs1_ind = dplyr::select(dfcfi[[u]],c("FVs0","ind"))
      if(!is.null(weights)){
        wt1 <- dfcfi[[u]]$wt
      }
      else{wt1 <- NULL}

      Y2 <- dfcfj[[u]]$Y
      FVs02 <- dfcfj[[u]]$FVs0
      FVs2_ind = dplyr::select(dfcfj[[u]],c("FVs0","ind"))
      if(!is.null(weights)){
        wt2 <- dfcfj[[u]]$wt
      }
      X2 <- dplyr::select(dfcfj[[u]][,-c(ncol(dfcfj[[u]]))], -c(Y,wt,FVs0))
      FVs1 <- ML::FVest(model,X,Y,X1,Y1,ML, polynomial.Lasso = polynomial.Lasso,
                        polynomial.Ridge = polynomial.Ridge,
                        polynomial.NLLS_exp = polynomial.NLLS_exp,
                        polynomial.loglin = polynomial.loglin,
                        coefs = coefs)
      FVs1_ind$FVs = FVs1
      FVs2 <- ML::FVest(model,X,Y,X2,Y2,ML, polynomial.Lasso = polynomial.Lasso,
                        polynomial.Ridge = polynomial.Ridge,
                        polynomial.NLLS_exp = polynomial.NLLS_exp,
                        polynomial.loglin = polynomial.loglin,
                        coefs = coefs)
      FVs2_ind$FVs = FVs2
      FVsinfold_bind = rbind(FVs1_ind,FVs2_ind)
      FVsinfold <- FVsinfold_bind[!duplicated(FVsinfold_bind$ind), ]
      FVs1 <- FVs1*(FVs1 > 0) + (FVs1 <= 0)
      FVs2 <- FVs2*(FVs2 > 0) + (FVs2 <= 0)
      if(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0) != 0){
        warning(paste(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0),
                      "FVs are lower or equal than 0."))
      }
      #Triangle
      if(u %in% c(1,2)){
        num <- iodnumtr(Y1,Y2,FVs1,FVs2,wt1 = wt1, wt2 = wt2, FVs01 = FVs01, FVs02 = FVs02)
        sgns <- num[[2]]
        num <- num[[1]]
        #Compute RMSE of first stage
        RMSE1 <- (length(Y1)/length(Y))*
          sqrt(weighted.mean2(((Y1 - FVs1)^2),wt1))
      #Square
      } else{
        num <- iodnumsq(Y1,FVs1,Y2,FVs2, wt1 = wt1, wt2 = wt2, FVs01 = FVs01, FVs02 = FVs02)
        sgns <- num[[2]]
        num <- num[[1]]
        RMSE1 = 0
      }
      return(list(nums = num, RMSE1s = RMSE1, coefs = coefs,
                  sgns = sgns, FVsinfold = FVsinfold))
      })
    resnums = sapply(res, function(t){
      t$nums
    })
    numcf = sum(resnums)

    resrmse = sapply(res, function(t){
      t$RMSE1s
    })
    RMSE1 = sum(resrmse[1:2])

    coefs = sapply(res, function(t){
      t$coefs
    })

    sgns = lapply(res, function(t){
      t$sgns
    })

    FVsinfold = lapply(res, function(t){
      t$FVsinfold
    })

    #Compute denominator
    n <- length(Y)
    if (is.null(weights) == 1){
      dencf <- (n-1)*n*mean(Y)
    }
    else{
      WW <- sumUw(weights)
      dencf <- WW*2*stats::weighted.mean(Y,weights)
    }
    #Estimate and RMSE1
    iod_gini <- numcf/dencf
    RMSE1 <- sum(RMSE1)
    #FVs
    if (fitted_values == TRUE | sterr == TRUE){
      if(is.null(extFVs)){
        m <- ML::MLest(X, Y, ML, OLSensemble = OLSensemble,
                       SL.library = SL.library,
                       ensemblefolds = ensemblefolds,
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
                       FVs = TRUE,
                       weights = weights)

        FVres <- m$FVs
      }
      if(!is.null(extFVs)){
        FVres = extFVs
      }
      if(sum(FVres <= 0) != 0){
        warning(paste(sum(FVres <= 0),"FVs are lower or equal than 0."))
      }
    }
    else if (fitted_values == FALSE & sterr == FALSE){
      FVres <- NULL
    }
    #SE
    if (sterr == TRUE){
        se_gini <- se_deb(Y, FVres, iod_gini, weights = weights)
    } else{
      se_gini <- NULL
    }
    #IOp relative
    if (IOp_rel == TRUE){
      G <- acid::weighted.gini(Y,weights)
      G <- as.numeric(G[2])
      IOpr_gini <- iod_gini/G
      if (sterr == TRUE){
        se_rel_gini <- se_rel(Y,FVres,iod_gini, IY = G, weights = weights)
      } else{se_rel_gini <- NULL}
    } else{
      IOpr_gini <- NULL
      se_rel_gini <- NULL
    }
    jt <- rbind("IOp" = iod_gini, "se" = se_gini)
    jtrel <- rbind("IOp_rel" = IOpr_gini, "se" = se_rel_gini)
    jt = cbind(jt,jtrel)
    if (IOp_rel == TRUE){
      colnames(jt) = c("IOp","IOp_rel")
    } else{
      colnames(jt) = c("IOp")
    }
    if (fitted_values == TRUE){
      if (ML == "OLSensemble"){
        return(list(IOp = jt, RMSE1 = RMSE1,
                    FVs = FVres,
                    FVsinfold = FVsinfold,
                    coefs = coefs,
                    sgns = sgns))
      } else{
        return(list(IOp = jt, RMSE1 = RMSE1,
                    FVs = FVres,
                    FVsinfold = FVsinfold,
                    sgns = sgns))
      }
    } else{
      if (ML == "OLSensemble"){
        return(list(IOp = jt, RMSE1 = RMSE1, coefs = coefs,
                    FVsinfold = FVsinfold,
                    sgns = sgns))
      } else{
        return(list(IOp = jt, RMSE1 = RMSE1,
                    FVsinfold = FVsinfold,
                    sgns = sgns))
      }
    }
  }
}
