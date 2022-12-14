IOPI <- function(Y,
                 X,
                 ineq = c("Gini", "MLD",c("Gini","MLD")),
                 plugin_method = c("loglin", "NP", "ML"),
                 ML = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                 ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                 IOp_rel = FALSE,
                 sterr = FALSE,
                 fitted_values = FALSE,
                 boots = 100,
                 weights = NULL){
  #Log linear
  if (plugin_method == "loglin"){
    iopi <- logliniop(X,
                      Y,
                      ineq = ineq,
                      IOp_rel = IOp_rel,
                      fitted_values = fitted_values,
                      weights = weights)
    if (sterr == TRUE){
      seres <- sapply(1:boots, function(u){
        ind <- sample(length(Y), replace = TRUE)
        Yboot <- Y[ind]
        Xboot <- X[ind,]
        logliniop(Xboot,
                  Y,
                  ineq = ineq,
                  IOp_rel = IOp_rel,
                  fitted_values = FALSE,
                  weights = weights)
      })
    }
  }
  #NP
  else if (plugin_method == "NP"){
    iopi <- npiop(X,
                  Y,
                  ineq = ineq,
                  IOp_rel = IOp_rel,
                  fitted_values = fitted_values,
                  weights = weights)
    if (sterr == TRUE){
      seres <- sapply(1:boots, function(u){
        ind <- sample(length(Y), replace = TRUE)
        Yboot <- Y[ind]
        Xboot <- X[ind,]
        npiop(Xboot,
              Yboot,
              ineq = ineq,
              IOp_rel = IOp_rel,
              fitted_values = FALSE,
              weights = weights)
      })
    }
  }
  #ML
  else if (plugin_method == "ML"){
    #method takes the value of ML
    iopi <- mliop(X,
                  Y,
                  ML = ML,
                  ensemble = ensemble,
                  ineq = ineq,
                  IOp_rel = IOp_rel,
                  fitted_values = fitted_values,
                  weights = weights)
      #Standard errors
      if(sterr == TRUE){
        seres <- sapply(1:boots, function(u){
          ind <- sample(length(Y), replace = TRUE)
          Yboot <- Y[ind]
          Xboot <- X[ind,]
          mliop(Xboot,
                Yboot,
                ML = ML,
                ensemble = ensemble,
                ineq = ineq,
                IOp_rel = IOp_rel,
                fitted_values = FALSE,
                weights = weights)
        })
      }
  }
  if (fitted_values == TRUE){
    FVs <- iopi$FVs
    iopi <- iopi$IOp
  }
  if (sterr == TRUE){
    if (!is.null(nrow(seres)) == 1){
      se <- sapply(1:nrow(seres), function(u){sd(seres[u,])})
    } else{
      se <- sd(seres)
    }
    if (IOp_rel == TRUE){
      aux <- sapply(ineq,function(u){paste(u,"rel",sep = "_")})
      names(se) <- c(rbind(ineq,aux))
      iopi <- c(iopi)
      names(iopi) <- c(rbind(ineq,aux))
    } else{
      names(se) <- ineq
      iopi <- c(iopi)
      names(iopi) <- ineq
    }
  } else{
    se <- NULL
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
    IOp_res <- rbind(iopi[ineq],se[ineq])
    IOp_rel_res <- rbind(iopi[paste(ineq,"rel",sep = "_")],
                         se[paste(ineq,"rel",sep = "_")])
    colnames(IOp_rel_res) <- ineq
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
    IOp_res <- rbind(iopi[ineq],se[ineq])
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

