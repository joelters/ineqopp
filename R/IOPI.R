IOPI <- function(Y,
                 X,
                 ineq = c("Gini", "MLD",c("Gini","MLD")),
                 ML = c("Lasso","Ridge","RF","CIF","XGB","CB",
                        "OLSensemble","SL"),
                 ensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                 ensemblefolds = 2,
                 IOp_rel = FALSE,
                 sterr = FALSE,
                 fitted_values = TRUE,
                 weights = NULL,
                 rf.cf.ntree = 500,
                 rf.depth = NULL,
                 polynomial = 1){
  if(!is.null(weights) & class(weights) != "numeric"){
    stop("Weights have to be numeric")
  }
  iopi <- mliop(X,
                Y,
                ML = ML,
                ensemble = ensemble,
                ensemblefolds = ensemblefolds,
                ineq = ineq,
                IOp_rel = IOp_rel,
                fitted_values = fitted_values,
                weights = weights,
                rf.cf.ntree = rf.cf.ntree,
                rf.depth = rf.depth,
                polynomial = polynomial)
    #Standard errors
  if(sterr == TRUE){
      warning("For se with plug in set FVs = TRUE")
      FVs <- iopi$FVs
      se <- sepi(Y,FVs,iopi$IOp, ineq = ineq, weights = weights)
  }

  if (fitted_values == TRUE){
    FVs <- iopi$FVs
    iopi <- iopi$IOp
  }
  if (sterr == TRUE){
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

