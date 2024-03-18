IOD3 <- function(Y,
                 X,
                 CFit = TRUE,
                 npart = 5,
                 ineq = c("Gini", "MLD",c("Gini", "MLD")),
                 ML = c("Lasso","Ridge","RF","CIF","XGB","CB","SL"),
                 ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                 sterr = TRUE,
                 weights = NULL,
                 IOp_rel = FALSE,
                 fitted_values = FALSE,
                 parallel = FALSE,
                 rf.cf.ntree = 500,
                 rf.depth = NULL,
                 verbose = FALSE){
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
                   ensemble = ensemble,
                   rf.cf.ntree = rf.cf.ntree,
                   rf.depth = rf.depth,
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
        se <- se_deb3(Y, FVs, iodeb, ineq = u, weights = weights)
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
    dfcf <- SP(df,npart)
    dfcf <- dfcf$dfsp
    if ("Gini" %in% ineq){
      numcf <- 0
      # SM <- vector(mode = "list", npart)
      count <- 0
      cnt <- 0
      iod_mld <- 0
      RMSE1 <- rep(0,npart)
      CFind <- NULL
      for (i in 1:npart){
        count <- count + 1
        j1 <- i
        for (j in j1:npart){
          cnt <- cnt + 1
          CFind[[cnt]] <- c(i,j,cnt,count)
        }
      }
      if (parallel == FALSE){
        rescf <- lapply(CFind,function(u){
          i <- u[1]
          j <- u[2]
          cnt <- u[3]
          count <- u[4]

          #Create dataframe with all observations not in I_l
          aux <- dfnotl(dfcf,i,j)

          #Train model with observations not in I_l (not in K = i or K = j)
          m <- ML::MLest(dplyr::select(aux,-c(Y,wt)),
                         aux$Y,
                         ML,
                         ensemble = ensemble,
                         rf.cf.ntree = rf.cf.ntree,
                         rf.depth = rf.depth,
                         FVs = FALSE,
                         weights = aux$wt)
          model <- m$model

          #Predict fitted values for obs in Ci and Cj
          #using model trained with obs not in Ci or Cj
          #Y1 are income observations in Ci, FVs1 are
          #predictions on Ci with model trained with obs
          #not in Ci or Cj
          #Y2 are income observations in Cj, FVs2 are
          #predictions on Cj with model trained with obs
          #not in Ci or Cj

          #i <= j (we want to sum across all i,j, but we are
          #doing i <= j in the loop, we are going to do
          #i > j manually whenever in the loop i!=j by just
          #switching the roles of i and j, this way we dont
          #train the model twice (once for (i,j) and another for (j,i)))
          X1 <- dplyr::select(dfcf[[i]], -c(Y,wt))
          Y1 <- dfcf[[i]]$Y
          if(!is.null(weights)){
            wt1 <- dfcf[[i]]$wt
          }
          else{wt1 <- NULL}

          #If we are in a square
          Y2 <- dfcf[[j]]$Y
          if(!is.null(weights)){
            wt2 <- dfcf[[j]]$wt
          }
          X2 <- dplyr::select(dfcf[[j]], -c(Y,wt))
          FVs1 <- round(ML::FVest(model,X,Y,X1,Y1,ML),7) #we round to avoid floating issues with sign function
          FVs2 <- round(ML::FVest(model,X,Y,X2,Y2,ML),7)
          FVs1 <- FVs1*(FVs1 > 0) + (FVs1 <= 0)
          FVs2 <- FVs2*(FVs2 > 0) + (FVs2 <= 0)
          if(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0) != 0){
            warning(paste(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0),
                          "FVs were lower or equal than 0 and were
                  turned into the value 1."))
          }
          #Rank of i across j
          rk <- sapply(FVs1, function(u) sum(FVs2 <= u)/length(FVs2))
          concind <- weighted.mean(rk*Y1, wt1) - 0.5*weighted.mean(Y1,wt1)

          # i > j
          #Rank of i across j
          if (i !=j){
            rk <- sapply(FVs2, function(u) sum(FVs1 <= u)/length(FVs1))
            concind <- concind + weighted.mean(rk*Y2, wt2) - 0.5*weighted.mean(Y2,wt2)
          }
          # save zeros for FVs and RMSE since we only want to save them on diagonal
          fvss <- rep(0,length(Y1))
          RMSE1 <- 0
          num_mld <- 0
          #Compute RMSE of first stage
          if (i == j){
            RMSE1 <- (length(Y1)/length(Y))*
              sqrt(weighted.mean2(((Y1 - FVs1)^2),wt1))
            fvss <- FVs1
            if ("MLD" %in% ineq){
              #weights for MLD (1/n or weights)
              wtmld <- dfcf[[i]]$wt
              num_mld <- sum(wtmld*(log(FVs1) + (1/FVs1)*(Y1-FVs1)))
            }
            if (verbose == TRUE){
              print(paste(round(100*cnt/(npart*(npart + 1)/2),2),"% completed"))
            }
          }
          #Add the term in the numerator of the estimator
          #corresponding to this block
          # numcf <- numcf + num
          if ("MLD" %in% ineq){
            return(list("concrmse" = data.frame("concind" = concind,
                                                "num_mld" = num_mld,
                                                "RMSE1" = RMSE1),
                        FVs = fvss))
          } else{
            return(list("concrmse" = data.frame("concind" = concind,
                                                "RMSE1" = RMSE1),
                        FVs = fvss))
          }
        })
      }
      else if (parallel == TRUE){
        n.cores <- parallel::detectCores()
        clust <- parallel::makeCluster(n.cores)
        parallel::clusterEvalQ(clust, set.seed(123))
        parallel::clusterExport(clust, c("dfcf","npart","rf.cf.ntree",
                                         "rf.depth"),
                                envir=environment())
        rescf <- parallel::parLapply(clust, CFind, function(u){
           i <- u[1]
           j <- u[2]
           cnt <- u[3]
           count <- u[4]

           #Create dataframe with all observations not in I_l
           aux <- dfnotl(dfcf,i,j)

           #Train model with observations not in I_l (not in K = i or K = j)
           m <- ML::MLest(dplyr::select(aux,-c(Y,wt)),
                          aux$Y,
                          ML,
                          ensemble = ensemble,
                          rf.cf.ntree = rf.cf.ntree,
                          rf.depth = rf.depth,
                          FVs = FALSE,
                          rf.cf.ntree = rf.cf.ntree,
                          rf.depth = rf.depth,
                          weights = aux$wt)
           model <- m$model

           #Predict fitted values for obs in Ci and Cj
           #using model trained with obs not in Ci or Cj
           #Y1 are income observations in Ci, FVs1 are
           #predictions on Ci with model trained with obs
           #not in Ci or Cj
           #Y2 are income observations in Cj, FVs2 are
           #predictions on Cj with model trained with obs
           #not in Ci or Cj

           #i <= j (we want to sum across all i,j, but we are
           #doing i <= j in the loop, we are going to do
           #i > j manually whenever in the loop i!=j by just
           #switching the roles of i and j, this way we dont
           #train the model twice (once for (i,j) and another for (j,i)))
           X1 <- dplyr::select(dfcf[[i]], -c(Y,wt))
           Y1 <- dfcf[[i]]$Y
           if(!is.null(weights)){
             wt1 <- dfcf[[i]]$wt
           }
           else{wt1 <- NULL}

           #If we are in a square
           Y2 <- dfcf[[j]]$Y
           if(!is.null(weights)){
             wt2 <- dfcf[[j]]$wt
           }
           X2 <- dplyr::select(dfcf[[j]], -c(Y,wt))
           FVs1 <- round(ML::FVest(model,X,Y,X1,Y1,ML),7) #we round to avoid floating issues with sign function
           FVs2 <- round(ML::FVest(model,X,Y,X2,Y2,ML),7)
           FVs1 <- FVs1*(FVs1 > 0) + (FVs1 <= 0)
           FVs2 <- FVs2*(FVs2 > 0) + (FVs2 <= 0)
           if(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0) != 0){
             warning(paste(sum(m$FVs1 <= 0) + sum(m$FVs1 <= 0),
                           "FVs were lower or equal than 0 and were
                            turned into the value 1."))
           }
           #Rank of i across j
           rk <- sapply(FVs1, function(u) sum(FVs2 <= u)/length(FVs2))
           concind <- weighted.mean(rk*Y1, wt1) - 0.5*weighted.mean(Y1,wt1)

           # i > j
           #Rank of i across j
           if (i !=j){
             rk <- sapply(FVs2, function(u) sum(FVs1 <= u)/length(FVs1))
             concind <- concind + weighted.mean(rk*Y2, wt2) - 0.5*weighted.mean(Y2,wt2)
           }
           # save zeros for FVs and RMSE since we only want to save them on diagonal
           fvss <- rep(0,length(Y1))
           RMSE1 <- 0
           num_mld <- 0
           #Compute RMSE of first stage
           if (i == j){
             RMSE1 <- (length(Y1)/length(Y))*
               sqrt(weighted.mean2(((Y1 - FVs1)^2),wt1))
             fvss <- FVs1
             if ("MLD" %in% ineq){
               #weights for MLD (1/n or weights)
               wtmld <- dfcf[[i]]$wt
               num_mld <- sum(wtmld*(log(FVs1) + (1/FVs1)*(Y1-FVs1)))
             }
             if (verbose == TRUE){
               print(paste(round(100*count/(npart*(npart + 1)/2),2),"% completed"))
             }
           }
           #Add the term in the numerator of the estimator
           #corresponding to this block
           # numcf <- numcf + num
           if ("MLD" %in% ineq){
             return(list("concrmse" = data.frame("concind" = concind,
                                                 "num_mld" = num_mld,
                                                 "RMSE1" = RMSE1),
                         FVs = fvss))
           } else{
              return(list("concrmse" = data.frame("concind" = concind,
                                               "RMSE1" = RMSE1),
                          FVs = fvss))
           }
         })
        parallel::stopCluster(clust)
      }
      rescf2 <- lapply(1:length(rescf), function(u) rescf[[u]][[1]])
      rescf3 <- do.call(rbind,rescf2)
      iii <- which(rescf3$RMSE1 != 0)
      rescf4 <- lapply(1:length(rescf), function(u) rescf[[u]][[2]])
      FVs <- unlist(rescf4[iii])

      concind <- sum(rescf3$concind)
      RMSE1 <- sum(rescf3$RMSE1)
      if ("MLD" %in% ineq){
        iod_mld <- sum(rescf3$num_mld)
      }

      #Compute denominator
      n <- length(Y)
      if (is.null(weights) == 1){
        dencf <- 2*mean(Y)*npart^2
      }
      else{
        dencf <- 2*weighted.mean(Y,weights)*npart^2
      }
      #Estimate and RMSE1
      iod_gini <- 4*concind/dencf
      if ("MLD" %in% ineq){
        iod_mld <- log(stats::weighted.mean(Y,weights)) - iod_mld
      }

      #FVs
      if (fitted_values == TRUE | sterr == TRUE){
        m <- ML::MLest(X, Y, ML, ensemble = ensemble, FVs = TRUE,
                       rf.cf.ntree = rf.cf.ntree,
                       rf.depth = rf.depth,
                       weights = weights)
        FVres <- round(m$FVs,7) #we round to avoid floating issues with sign function
        FVres <- FVres*(FVres > 0) + (FVres <= 0)
        # FVres <- FVs
      } else if (fitted_values == FALSE & sterr == FALSE){
        FVres <- NULL
      }
      #SE
      if (sterr == TRUE){
        if (verbose == TRUE){
          print("Computing standard error")
        }
        se_gini <- se_deb3(Y, FVres, iod_gini, ineq = "Gini", weights = weights)
        if ("MLD" %in% ineq){
          se_mld <- se_deb3(Y, FVres, iod_mld, ineq = "MLD", weights = weights)
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
          if (verbose == TRUE){
            print("Computing se of relative estimate")
          }
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
                                      "se" = c(se_rel_gini, se_rel_mld)), FVs = fvss))
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
          return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel, FVs = FVres))
        } else{
          return(list(IOp = jt, RMSE1 = RMSE1, IOp_rel = jtrel))
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
      fvss <- NULL
      for (i in 1:npart){
        #Create dataframe with all observations not in C_i
        aux <- dfnotl(dfcf,i,i)

        #Train model with observations not in I_l
        m <- ML::MLest(dplyr::select(aux,-c(Y,wt)),
                       aux$Y,
                       ML,
                       ensemble = ensemble,
                       FVs = FALSE,
                       rf.cf.ntree = rf.cf.ntree,
                       rf.depth = rf.depth,
                       weights = aux$wt)
        model <- m$model

        #Predict fitted values for obs in Ci
        #using model trained with obs not in Ci
        Xcf <- dplyr::select(dfcf[[i]], -c(Y,wt))
        Ycf <- dfcf[[i]]$Y
        wtcf <- dfcf[[i]]$wt

        #Estimate fitted values
        FVs <- ML::FVest(model, X, Y, Xcf, Ycf, ML)
        FVs <- (FVs*(FVs > 0) + (FVs <= 0))
        fvss <- c(fvss, FVs)
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
        # FVres <- fvss
        m <- ML::MLest(X, Y, ML, ensemble = ensemble, FVs = TRUE,
                       rf.cf.ntree = rf.cf.ntree,
                       rf.depth = rf.depth,
                       weights = weights)
        FVres <- round(m$FVs,7) #we round to avoid floating issues with sign function
        FVres <- FVres*(FVres > 0) + (FVres <= 0)
      } else if (fitted_values == FALSE & sterr == FALSE){
        FVres <- NULL
      }
      #SE
      if (sterr == TRUE){
        se <- se_deb3(Y, FVres, iodeb, ineq = ineq, weights = weights)
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
