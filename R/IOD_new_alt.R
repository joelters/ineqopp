IOD_new_alt <- function(Y,
                    X,
                    CFit = TRUE,
                    npart = 5,
                    ineq = c("Gini", "MLD",c("Gini", "MLD")),
                    ML = c("Lasso","Ridge","RF","CIF","XGB","CB",
                           "OLSensemble", "SL"),
                    OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                    SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                    ensemblefolds = 5,
                    sterr = TRUE,
                    sterr_type = 1,
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
                    cb.depth = 6){
  if(!is.null(weights) & class(weights) != "numeric"){
    stop("Weights have to be numeric")
  }
  #method takes the value of RF_type
  ML = match.arg(ML)
  # ineq = match.arg(ineq)
  if (CFit == FALSE){
    #Model and FVs estimation
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
                   FVs = TRUE,
                   weights = weights)
    model <- m$model
    #we round to avoid floating issues with sign function
    FVs <- round(m$FVs,7)*(m$FVs > 0) + 1*(m$FVs > 0)
    if(sum(m$FVs <= 0) != 0){
      warning(paste(sum(m$FVs <= 0),"FVs were lower or equal than 0 and were
                    turned into the value 1."))
    }
    res <- sapply(ineq, function(u){
      #Debiased IOp
      iodeb <- io_deb(Y, FVs, ineq = u, weights = weights)
      #SEs
      if (sterr == TRUE){
        if (sterr_type == 1){
          se <- se_deb(Y, FVs, iodeb, ineq = u, weights = weights)
        }
        else if (sterr_type == 2){
          se <- se_deb_unb(Y, FVs, iodeb, ineq = u, weights = weights)
        }
      } else{se <- NULL}
      if (u == "Gini" & IOp_rel == TRUE){
        G <- acid::weighted.gini(Y,weights)
        G <- as.numeric(G[2])
        iorel <- iodeb/G
        return(c(IOp = iodeb, IOp_rel = iorel, se = se))
      }
      else if (u == "MLD" & IOp_rel == TRUE){
        MLD <- mld(Y,weights)
        iorel <- iodeb/MLD
        return(c(IOp = iodeb, IOp_rel = iorel, se = se))
      }
      else if (IOp_rel == FALSE){return(c(IOp = iodeb, se = se))}
    })
    if (IOp_rel == TRUE){
      res_rel <- rbind(res["IOp_rel",],NULL)
      rownames(res_rel) <- "IOp_rel"
      colnames(res_rel) <- ineq
      if (sterr == TRUE){
        resiop <- rbind(res["IOp",],res["se",])
        rownames(resiop) <- c("IOp","se")
      }
      else {
        resiop <- rbind(res["IOp",],NULL)
        rownames(resiop) <- c("IOp")
      }
    } else{
      res_rel <- NULL
      if (sterr == TRUE){
        resiop <- rbind(res["IOp",],res["se",])
        rownames(resiop) <- c("IOp","se")
      }
      else {
        resiop <- rbind(res,NULL)
        rownames(resiop) <- "IOp"
      }
    }
    colnames(resiop) <- ineq
    if (fitted_values == TRUE){
      return(list(IOp = resiop, IOp_rel = res_rel, FVs = FVs))
    }
    else{
      return(list(IOp = resiop, IOp_rel = res_rel))
    }
  }
  else{
    nn <- length(Y)
    weights <- if (is.null(weights)) rep(1/nn,nn) else weights
    df <- dplyr::as_tibble(cbind(cbind(Y = Y, wt = weights),X))
    names(df)[1] = "Y"
    # reshufle data in case there is some order
    df = df[sample(1:nn),]
    rownames(df) = paste(1:nn)
    dfcf <- SP_new(df)
    dfcfi <- dfcf$dfcfi
    dfcfj <- dfcf$dfcfj
    if ("Gini" %in% ineq){
      numcf <- 0
      iod_mld <- 0
      res = lapply(1:6, function(u){
        #Create dataframe with all observations not in I_l
        aux <- dfnotl_new(df,dfcfi,dfcfj,u)

        #Train model with observations not in I_l (not in K = i or K = j)
        m <- ML::MLest(dplyr::select(aux,-c(Y,wt)),
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
                       FVs = TRUE,
                       weights = aux$wt)
        in.sample.preds = m$FVs
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

        X1 <- dplyr::select(dfcfi[[u]][,-c(ncol(dfcfi[[u]]))], -c(Y,wt))
        Y1 <- dfcfi[[u]]$Y
        if(!is.null(weights)){
          wt1 <- dfcfi[[u]]$wt
        }
        else{wt1 <- NULL}

        Y2 <- dfcfj[[u]]$Y
        if(!is.null(weights)){
          wt2 <- dfcfj[[u]]$wt
        }

        X2 <- dplyr::select(dfcfj[[u]][,-c(ncol(dfcfj[[u]]))], -c(Y,wt))
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
          mean((FVs1[u] > in.sample.preds) - (in.sample.preds > FVs1[u]))
        })
        alpha2 = sapply(1:length(FVs2), function(u){
          mean((FVs2[u] > in.sample.preds) - (in.sample.preds > FVs2[u]))
        })
        #Triangle
        if(u %in% c(1,2)){
          num <- iodnumtr_new_alt(Y1,Y2,FVs1,FVs2, a1 = alpha1, a2 = alpha2, wt1 = wt1, wt2 = wt2)
          if ("MLD" %in% ineq){
            num_mld <- sum(wtmld*(log(FVs1) + (1/FVs1)*(Y1-FVs1)))
            iod_mld <- iod_mld + num_mld
          }
          #Compute RMSE of first stage
          RMSE1 <- (length(Y1)/length(Y))*
            sqrt(weighted.mean2(((Y1 - FVs1)^2),wt1))
          #Square
        } else{
          num <- iodnumsq_alt(Y1,FVs1,Y2,FVs2, a1 = alpha1, a2 = alpha2, wt1 = wt1, wt2 = wt2)
          RMSE1 = 0
        }
        return(list(nums = num, RMSE1s = RMSE1, coefs = coefs))
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
      if ("MLD" %in% ineq){
        iod_mld <- log(stats::weighted.mean(Y,weights)) - iod_mld
      }
      RMSE1 <- sum(RMSE1)
      #FVs
      if (fitted_values == TRUE | sterr == TRUE){
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
                       FVs = TRUE,
                       weights = weights)

        FVres <- m$FVs
        FVres <- FVres*(FVres > 0) + (FVres <= 0)
        alpha <- sapply(1:length(FVres), function(u){
          mean(sign(FVres[u] - FVres[-u]))
        })
        if(sum(m$FVres <= 0) != 0){
          warning(paste(sum(m$FVres <= 0),"FVs were lower or equal than 0 and were
                      turned into the value 1."))
        }
      } else if (fitted_values == FALSE & sterr == FALSE){
        FVres <- NULL
      }
      #SE
      if (sterr == TRUE){
        if (sterr_type == 1){
          se_gini <- se_deb_alt(Y, FVres, alpha, iod_gini, ineq = "Gini", weights = weights)
        }
        else if (sterr_type == 2){
          se_gini <- se_deb_unb(Y, FVres, iod_gini, ineq = "Gini", weights = weights)
        }
        if ("MLD" %in% ineq){
          se_mld <- se_deb(Y, FVres, iod_mld, ineq = "MLD", weights = weights)
        }
      } else{
        se_gini <- NULL
        if ("MLD" %in% ineq){
          se_mld <- NULL
        }
      }
      #IOp relative
      if (IOp_rel == TRUE){
        G <- acid::weighted.gini(Y,weights)
        G <- as.numeric(G[2])
        IOpr_gini <- iod_gini/G
        if (sterr == TRUE){
          se_rel_gini <- se_rel(Y,FVres,iod_gini, ineq = ineq, IY = G, weights = weights)
        } else{se_rel_gini <- NULL}
        if ("MLD" %in% ineq){
          IY_mld <- mld(Y,weights)
          IOpr_mld <- iod_mld/IY_mld
          if (sterr == TRUE){
            se_rel_mld <- se_rel(Y,FVres,iod_mld, ineq = ineq, IY = IY_mld, weights = weights)
          } else {se_rel_mld <- NULL}
        }
      } else{
        IOpr_gini <- NULL
        se_rel_gini <- NULL
        if ("MLD" %in% ineq){
          IOpr_mld <- NULL
          se_rel_mld <- NULL
        }
      }
      if ("MLD" %in% ineq){
        if (fitted_values == TRUE){
          return(list(IOp = rbind("IOp" = c("Gini" = iod_gini, "MLD" = iod_mld),
                                  "se" = c(se_gini, se_mld)),
                      RMSE1 = RMSE1,
                      IOp_rel = rbind("IOp_rel" = c("Gini" = IOpr_gini, "MLD" = IOpr_mld),
                                      "se" = c(se_rel_gini, se_rel_mld)), FVs = FVres))
        }else{
          return(list(IOp = rbind("IOp" = c("Gini" = iod_gini, "MLD" = iod_mld),
                                  "se" = c(se_gini, se_mld)),
                      RMSE1 = RMSE1,
                      IOp_rel = rbind("IOp_rel" = c("Gini" = IOpr_gini, "MLD" = IOpr_mld),
                                      "se" = c(se_rel_gini, se_rel_mld))))
        }
      }
      else {
        jt <- rbind("IOp" = iod_gini, "se" = se_gini)
        colnames(jt) <- "Gini"
        jtrel <- rbind("IOp_rel" = IOpr_gini, "se" = se_rel_gini)
        if (IOp_rel == TRUE){
          colnames(jtrel) <- "Gini"
        }
        if (fitted_values == TRUE){
          return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel, FVs = FVres, coefs = coefs))
        } else{
          return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel, coefs = coefs))
        }
      }
    }
    else if ("MLD" %in% ineq & !"Gini" %in% ineq){
      nn <- length(Y)
      weights <- if (is.null(weights)) rep(1/nn,nn) else weights
      df <- dplyr::as_tibble(cbind(cbind(Y = Y, wt = weights),X))
      dfcf <- SP(df,npart)
      indices <- dfcf$indices
      dfcf <- dfcf$dfsp
      iodeb <- 0
      RMSE1 <- rep(0,npart)
      count <- 0
      for (i in 1:npart){
        #Create dataframe with all observations not in C_i
        aux <- dfnotl(dfcf,i,i)

        #Train model with observations not in I_l
        m <- ML::MLest(dplyr::select(aux,-c(Y,wt)),
                       aux$Y,
                       ML,
                       OLSensemble = OLSensemble,
                       SL.library = SL.library,
                       ensemblefolds = ensemblefolds,
                       rf.cf.ntree = rf.cf.ntree,
                       rf.depth = rf.depth,
                       mtry = mtry,
                       polynomial.Lasso = polynomial.Lasso,
                       polynomial.Ridge = polynomial.Ridge,
                       FVs = FALSE,
                       weights = aux$wt)
        model <- m$model

        #Predict fitted values for obs in Ci
        #using model trained with obs not in Ci
        Xcf <- dplyr::select(dfcf[[i]], -c(Y,wt))
        Ycf <- dfcf[[i]]$Y
        wtcf <- dfcf[[i]]$wt

        #Estimate fitted values
        FVs <- ML::FVest(model, X, Y, Xcf, Ycf, ML,
                         polynomial.Lasso = polynomial.Lasso,
                         polynomial.Ridge = polynomial.Ridge)
        FVs <- FVs*(FVs > 0) + (FVs <= 0)
        if(sum(m$FVs <= 0) != 0){
          warning(paste(sum(m$FVs <= 0),"FVs were lower or equal than 0 and were
                    turned into the value 1."))
        }
        #term of sum of CF sample orthogonal moment
        num <- sum(wtcf*(log(FVs) + (1/FVs)*(Ycf-FVs)))
        #Compute RMSE of first stage
        count <- count + 1
        RMSE1[count] <- (length(Ycf)/length(Y))*
          sqrt(weighted.mean2(((Ycf - FVs)^2),wtcf))
        iodeb <- iodeb + num
      }
      iodeb <- log(stats::weighted.mean(Y,weights)) - iodeb
      RMSE1 <- sum(RMSE1)
      #FVs
      if (fitted_values == TRUE | sterr == TRUE){
        m <- ML::MLest(X, Y, ML, OLSensemble = OLSensemble, FVs = TRUE, weights = weights)
        FVres <- m$FVs
        FVres <- FVres*(FVres > 0) + (FVres <= 0)
        if(sum(m$FVres <= 0) != 0){
          warning(paste(sum(m$FVres <= 0),"FVs were lower or equal than 0 and were
                      turned into the value 1."))
        }
      } else if (fitted_values == FALSE & sterr == FALSE){
        FVres <- NULL
      }
      #SE
      if (sterr == TRUE){
        se <- se_deb(Y, FVres, iodeb, ineq = ineq, weights = weights)
      }
      else{se <- NULL}
      #IOp relative
      if (IOp_rel == TRUE){
        IY <- mld(Y,weights)
        IOpr <- iodeb/IY
        if (sterr == TRUE){
          se_rel <- se_rel(Y,FVres,iodeb, ineq = ineq, IY = IY, weights = weights)
        } else{se_rel <- NULL}
      } else{
        IOpr <- NULL
        se_rel <- NULL
      }
      jt <- rbind("MLD" = iodeb, "se" = se)
      jtrel <- rbind("MLD" = IOpr, "se" = se_rel)
      colnames(jt) <- "MLD"
      if (sterr == TRUE){
        rownames(jt) <- c("IOp","se")
      } else{rownames(jt) <- c("IOp")}
      if (IOp_rel == TRUE){
        colnames(jtrel) <- "MLD"
        if (sterr == TRUE){
          rownames(jtrel) <- c("IOp_rel","se")
        } else{rownames(jtrel) <- c("IOp_rel")}
      }
      if (fitted_values == TRUE){
        return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel, FVs = FVres))
      } else{
        return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel))
      }
    }
  }
}
