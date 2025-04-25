IOD_new2 <- function(Y,
                    X,
                    CFit = TRUE,
                    npart = 5,
                    ML = c("Lasso","Ridge","RF","CIF","XGB","CB", "Torch",
                           "OLSensemble", "SL"),
                    OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                    SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                    ensemblefolds = 5,
                    sterr = TRUE,
                    weights = NULL,
                    IOp_rel = FALSE,
                    fitted_values = FALSE,
                    rf.cf.ntree = 500,
                    rf.depth = NULL,
                    polynomial.Lasso = 1,
                    polynomial.Ridge = 1,
                    mtry = max(floor(ncol(X)/3), 1),
                    xgb.nrounds = 200,
                    xgb.max.depth = 6,
                    cb.iterations = 1000,
                    cb.depth = 6,
                    torch.epochs = 50,
                    torch.hidden_units = c(64, 32),
                    torch.lr = 0.01,
                    torch.dropout = 0.2,
                    FVs0 = NULL,
                    extFVs = NULL){
  if(!is.null(weights) & class(weights) != "numeric"){
    stop("Weights have to be numeric")
  }
  #method takes the value of RF_type
  ML = match.arg(ML)
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
    n = length(Y)
    alpha <- numeric(n)  # Preallocate memory
    for (u in 1:n) {
      alpha[u] <- (1 / (n-1)) * sum((FVs[u] > FVs[-u]) - (FVs[u] < FVs[-u]))
    }
    iodeb <- io_deb_new2(Y, FVs, alpha, weights = weights)
    #SEs
    if (sterr == TRUE){
      se <- se_deb_new2(Y, FVs, alpha, iodeb, weights = weights)
    } else{se <- NULL}
    if (IOp_rel == TRUE){
      G <- acid::weighted.gini(Y,weights)
      G <- as.numeric(G[2])
      iorel <- iodeb/G
      se_rel <- se_rel_new2(Y,FVs, alpha, iodeb, IY = G, weights = weights)
      res = c(IOp = iodeb, IOp_rel = iorel, se = se, se_rel = se_rel)
    }
    else if (IOp_rel == FALSE){res = c(IOp = iodeb, se = se)}

    if (IOp_rel == TRUE){
      res_rel <- rbind(res["IOp_rel"],res["se_rel"])
      rownames(res_rel) <- c("IOp_rel", "se")
      colnames(res_rel) <- "Gini"
      if (sterr == TRUE){
        resiop <- rbind(res["IOp"],res["se"])
        rownames(resiop) <- c("IOp","se")
      }
      else {
        resiop <- rbind(res["IOp"],NULL)
        rownames(resiop) <- c("IOp")
      }
    } else{
      res_rel <- NULL
      if (sterr == TRUE){
        resiop <- rbind(res["IOp"],res["se"])
        rownames(resiop) <- c("IOp","se")
      }
      else {
        resiop <- rbind(res,NULL)
        rownames(resiop) <- "IOp"
      }
    }
    colnames(resiop) <- "Gini"
    if (fitted_values == TRUE){
      return(list(IOp = resiop, IOp_rel = res_rel, FVs = FVs))
    }
    else{
      return(list(IOp = resiop, IOp_rel = res_rel))
    }
  }
  else{
    nn <- length(Y)
    if (is.null(FVs0)){
      FVs0 = rep(0,nn)
    }
    weights <- if (is.null(weights)) rep(1/nn,nn) else weights
    df <- dplyr::as_tibble(cbind(cbind(Y = Y, wt = weights, FVs0 = FVs0),X))
    names(df)[1] = "Y"
    # reshufle data in case there is some order
    df = df[sample(1:nn),]
    rownames(df) = paste(1:nn)
    dfcf <- SP_new(df)
    dfcfi <- dfcf$dfcfi
    dfcfj <- dfcf$dfcfj

    numcf <- 0
    res = lapply(1:6, function(u){
      #Create dataframe with all observations not in I_l
      aux <- dfnotl_new(df,dfcfi,dfcfj,u)

      #Train model with observations not in I_l (not in K = i or K = j)
      m <- ML::MLest(dplyr::select(aux,-c(Y,wt,FVs0)),
                     aux$Y,
                     ML,
                     OLSensemble = OLSensemble,
                     SL.library = SL.library,
                     ensemblefolds = ensemblefolds,
                     rf.cf.ntree = rf.cf.ntree,
                     rf.depth = rf.depth,
                     polynomial.Lasso = polynomial.Lasso,
                     polynomial.Ridge = polynomial.Ridge,
                     mtry = mtry,
                     xgb.nrounds = xgb.nrounds,
                     xgb.max.depth = xgb.max.depth,
                     cb.iterations = cb.iterations,
                     cb.depth = cb.depth,
                     torch.epochs = torch.epochs,
                     torch.hidden_units = torch.hidden_units,
                     torch.lr = torch.lr,
                     torch.dropout = torch.dropout,
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
      if(!is.null(weights)){
        wt1 <- dfcfi[[u]]$wt
      }
      else{wt1 <- NULL}

      Y2 <- dfcfj[[u]]$Y
      FVs02 <- dfcfj[[u]]$FVs0
      if(!is.null(weights)){
        wt2 <- dfcfj[[u]]$wt
      }

      X2 <- dplyr::select(dfcfj[[u]][,-c(ncol(dfcfj[[u]]))], -c(Y,wt,FVs0))
      FVs1 <- ML::FVest(model,X,Y,X1,Y1,ML, polynomial.Lasso = polynomial.Lasso,
                        polynomial.Ridge = polynomial.Ridge,
                        coefs = coefs)
      FVs2 <- ML::FVest(model,X,Y,X2,Y2,ML, polynomial.Lasso = polynomial.Lasso,
                        polynomial.Ridge = polynomial.Ridge,
                        coefs = coefs)
      FVs1 <- FVs1*(FVs1 > 0) + (FVs1 <= 0)
      FVs2 <- FVs2*(FVs2 > 0) + (FVs2 <= 0)
      if(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0) != 0){
        warning(paste(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0),
                      "FVs were lower or equal than 0 and were
                turned into the value 1."))
      }
      alpha1 = sapply(1:length(FVs1), function(u){
        mean((FVs1[u] > c(FVs1[-u],FVs2)) - (c(FVs1[-u],FVs2) > FVs1[u]))
      })
      alpha2 = sapply(1:length(FVs2), function(u){
        mean((FVs2[u] > c(FVs2[-u],FVs1)) - (c(FVs2[-u],FVs1) > FVs2[u]))
      })
      #Triangle
      if(u %in% c(1,2)){
        num <- iodnumtr_new2(Y1,Y2,FVs1,FVs2,alpha1,alpha2,
                             wt1 = wt1, wt2 = wt2, FVs01 = FVs01, FVs02 = FVs02)
        sgns <- num[[2]]
        num <- num[[1]]
        #Compute RMSE of first stage
        RMSE1 <- (length(Y1)/length(Y))*
          sqrt(weighted.mean2(((Y1 - FVs1)^2),wt1))
        #Square
      } else{
        num <- iodnumsq_new2(Y1,FVs1,Y2,FVs2, alpha1, alpha2,
                        wt1 = wt1, wt2 = wt2, FVs01 = FVs01, FVs02 = FVs02)
        sgns <- num[[2]]
        num <- num[[1]]
        RMSE1 = 0
      }
      return(list(nums = num, RMSE1s = RMSE1, coefs = coefs, sgns = sgns))
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
                       polynomial.Lasso = polynomial.Lasso,
                       polynomial.Ridge = polynomial.Ridge,
                       mtry = mtry,
                       xgb.nrounds = xgb.nrounds,
                       xgb.max.depth = xgb.max.depth,
                       cb.iterations = cb.iterations,
                       cb.depth = cb.depth,
                       torch.epochs = torch.epochs,
                       torch.hidden_units = torch.hidden_units,
                       torch.lr = torch.lr,
                       torch.dropout = torch.dropout,
                       FVs = TRUE,
                       weights = weights)

        FVres <- m$FVs
      }
      if(!is.null(extFVs)){
        FVres = extFVs
      }
      if(sum(FVres <= 0) != 0){
        warning(paste(sum(FVres <= 0),"FVs were lower or equal than 0 and were
                      turned into the value 1."))
      }
      FVres <- FVres*(FVres > 0) + (FVres <= 0)
      alpha <- numeric(n)
      for (u in 1:n) {
        alpha[u] <- (1 / (n-1)) * sum((fvs[u] > fvs[-u]) - (fvs[u] < fvs[-u]))
      }
    }
    else if (fitted_values == FALSE & sterr == FALSE){
      FVres <- NULL
    }
    #SE
    if (sterr == TRUE){
      se_gini <- se_deb_new2(Y, FVres, alpha, iod_gini, weights = weights)
    } else{
      se_gini <- NULL
    }
    #IOp relative
    if (IOp_rel == TRUE){
      G <- acid::weighted.gini(Y,weights)
      G <- as.numeric(G[2])
      IOpr_gini <- iod_gini/G
      if (sterr == TRUE){
        se_rel_gini <- se_rel_new2(Y,FVres, alpha, iod_gini, IY = G, weights = weights)
      } else{se_rel_gini <- NULL}
    } else{
      IOpr_gini <- NULL
      se_rel_gini <- NULL
    }

      jt <- rbind("IOp" = iod_gini, "se" = se_gini)
      colnames(jt) <- "Gini"
      jtrel <- rbind("IOp_rel" = IOpr_gini, "se" = se_rel_gini)
      if (IOp_rel == TRUE){
        colnames(jtrel) <- "Gini"
      }
      if (fitted_values == TRUE){
        return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel, FVs = FVres, coefs = coefs, sgns = sgns))
      } else{
        return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel, coefs = coefs,sgns = sgns))
      }

  }
}
