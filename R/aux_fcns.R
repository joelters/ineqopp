####### AUXILIARY FOR ESTIMATION #############
sepi <- function(Y,FVs,iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq = match.arg(ineq)
  if (ineq == "Gini"){
    if (is.null(weights)){
      n <- length(FVs)
      S <- 0
      aux2 <- 0
      for (i in 1:n){
        jt <- 0
        for (j in 1:n){
          # print(aux)
          jt <- jt +  (1/(n-1))*(i!=j)*(iop*(FVs[i] + FVs[j]) - abs(FVs[i] - FVs[j]))
        }
        S <- S + jt^2
      }
      B = 2*mean(Y)
      # S = n*((4*(n-1))/(n*((n-2)^2)))*S
      S = (4/n)*S
      V = (1/B^2)*S
      se = sqrt(V/n)
      return(se)
    }
    else{
      n <- length(FVs)
      S <- 0
      WW <- 0
      for (i in 1:n){
        jt <- 0
        for (j in 1:n){
          jt <- jt +  (1/(n-1))*(i!=j)*(iop*(FVs[i] + FVs[j]) - abs(FVs[i] - FVs[j]))
          WW <- WW + (j>i)*weights[i]*weights[j]
        }
        S <- S + (1/n)*jt^2
      }
      B = 2*weighted.mean(Y,weights)
      wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
      S = 4*S*sum(wt2^2)
      V = (1/B^2)*S
      se = sqrt(V)
      return(se)
    }
  }
  else if (ineq == "MLD"){
      nn <- length(FVs)
      wt <- if (is.null(weights)) rep(1/nn,nn) else weights
      th1 <- weighted.mean(FVs,wt)
      th2 <- weighted.mean(log(FVs),wt)
      S11 <- nn*sum(wt^2*(FVs - th1)^2)
      S12 <- nn*sum(wt^2*(FVs-th1)*(log(FVs)-th2))
      S22 <- nn*sum(wt^2*(log(FVs)- th2)^2)
      V <- S11/(th1^2) + S22 - 2*S12/th1
      se = sqrt(V/nn)
      return(se)
  }
}

io_deb <- function(Y, FVs, ineq = c("Gini", "MLD"), weights = NULL){
  ineq = match.arg(ineq)
  if (ineq == "Gini"){
    if (is.null(weights)){
      n <- length(Y)
      i1 <- n-1
      b1 <- 0
      for (i in 1:i1){
        j1 <- i+1
        for (j in j1:n){
          b1 <- b1 + ((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
            (Y[i] - Y[j])
        }
      }
      return(b1/((n-1)*n*mean(Y)))
    }
    else{
      n <- length(Y)
      b1 <- 0
      WW <- 0
      n1 <- n-1
      for (i in 1:n1){
        j1 <- i+1
        for (j in j1:n){
          b1 <- b1 + weights[i]*weights[j]*((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
            (Y[i] - Y[j])
          WW <- WW + weights[i]*weights[j]
        }
      }
      den <- WW*2*weighted.mean(Y,wt)
      return(b1/den)
    }
  }
  else if (ineq == "MLD"){
    wt <- if (is.null(weights)) rep(1/length(Y),length(Y)) else weights
    th1 <- weighted.mean(Y,wt)
    th2 <- weighted.mean(log(FVs) + (1/FVs)*(Y-FVs), wt)
    return(log(th1) - th2)
  }
}


se_deb <- function(Y, FVs, iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq <- match.arg(ineq)
  if (ineq == "Gini"){
    n <- length(Y)
    S <- 0
    if(is.null(weights)){
      for (i in 1:n){
        aux <- 0
        for (j in 1:n){
          jt <- (i!=j)*(iop*(Y[i] + Y[j]) -
                          ((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
                          (Y[i] - Y[j]))
          aux <- aux + (1/(n-1))*jt
        }
        S <- S + (1/n)*aux^2
      }
      vnum = 4*S
      V <- vnum/((2*mean(Y))^2)
      return(sqrt(V)/sqrt(length(Y)))
    }
    else{
      WW <- 0
      for (i in 1:n){
        aux <- 0
        for (j in 1:n){
          jt <- (i!=j)*(iop*(Y[i] + Y[j]) -
                          ((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
                          (Y[i] - Y[j]))
          aux <- aux + (1/(n-1))*jt
          WW <- WW + (i<j)*weights[i]*weights[j]
        }
        S <- S + (1/n)*aux^2
      }
      vnum = 4*S
      wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
      vnum = vnum*sum(wt2^2)
      vden <- (2*weighted.mean(Y,weights))^2
      V <- vnum/vden
      return(sqrt(V))
    }
  }
  else if (ineq == "MLD"){
    nn <- length(Y)
    wt <- if (is.null(weights)) rep(1/nn,nn) else weights
    th1 <- weighted.mean(Y,wt)
    th2 <- weighted.mean(log(FVs) + (1/FVs)*(Y-FVs), wt)
    S11 <- nn*sum(wt^2*(Y - th1)^2)
    S12 <- nn*sum(wt^2*(Y - th1)*(log(FVs) - th2 + (1/FVs)*(Y - FVs)))
    S22 <- nn*sum(wt^2*(log(FVs) - th2 + (1/FVs)*(Y - FVs))^2)
    V <- S11/(th1^2) + S22 - 2*S12/th1
    se <- sqrt(V/nn)
    return(se)
  }
}

se_rel <- function(Y,FVs,iodeb, ineq = c("Gini", "MLD"), IY, weights = NULL){
  n <- length(Y)
  S11 <- 0
  S22 <- 0
  S12 <- 0
  S <- 0
  if ("Gini" %in% ineq){
    if (is.null(weights)){
      for(i in 1:n){
        S11aux <- 0
        S22aux <- 0
        S12aux <- 0
        for (j in 1:n){
          S11aux <- S11aux + (i!=j)*(iodeb*(Y[i] + Y[j]) -
                                       ((FVs[i] - FVs[j] > 0) - (FVs[i] - FVs[j] < 0))*(Y[i] - Y[j]))
          S22aux <- S22aux + (i!=j)*(IY*(Y[i] + Y[j]) - abs(Y[i] - Y[j]))
        }
        S11 <- S11 + S11aux^2
        S22 <- S22 + S22aux^2
        S12 <- S12 + S11aux*S22aux
      }
      S11 <- (1/(n*((n-1)^2)))*S11
      S22 <- (1/(n*((n-1)^2)))*S22
      S12 <- (1/(n*((n-1)^2)))*S12
      V <- (1/(mean(Y)^2))*((S11/(IY^2)) + ((iodeb^2)/(IY^4))*S22 - 2*((iodeb)/(IY^3))*S12)
      se <- sqrt(V/n)
    }
    else{
      WW <- 0
      for(i in 1:n){
        S11aux <- 0
        S22aux <- 0
        S12aux <- 0
        for (j in 1:n){
          S11aux <- S11aux + (i!=j)*(iodeb*(Y[i] + Y[j]) -
                                       ((FVs[i] - FVs[j] > 0) - (FVs[i] - FVs[j] < 0))*(Y[i] - Y[j]))
          S22aux <- S22aux + (i!=j)*(IY*(Y[i] + Y[j]) - abs(Y[i] - Y[j]))
          WW <- WW + (j>i)*weights[i]*weights[j]
        }
        S11 <- S11 + S11aux^2
        S22 <- S22 + S22aux^2
        S12 <- S12 + S11aux*S22aux
      }
      S11 <- (1/(n*((n-1)^2)))*S11
      S22 <- (1/(n*((n-1)^2)))*S22
      S12 <- (1/(n*((n-1)^2)))*S12
      B = weighted.mean(Y,weights)
      wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
      V <- (1/(B^2))*((S11/(IY^2)) + ((iodeb^2)/(IY^4))*S22 - 2*((iodeb)/(IY^3))*S12)*sum(wt2^2)
      se <- sqrt(V)
    }
    return(se)
  }
  else if ("MLD" %in% ineq){
    nn <- length(Y)
    wt <- if (is.null(weights)) rep(1/nn,nn) else weights
    th1 <- weighted.mean(Y,wt)
    th2 <- weighted.mean(log(FVs) + (1/FVs)*(Y-FVs), wt)
    th3 <- weighted.mean(log(Y), wt)

    PSI <- wt*matrix(c(Y - th1, log(FVs) - th2 + (1/FVs)*(Y - FVs),
                    log(Y) - th3),nn,3)
    S <- t(PSI) %*% PSI
    grad <- c(((1/th1)*(th2 - th3))/(log(th1) - th3)^2,
              -1/(log(th1) - th3),
              (log(th1) - th2)/(log(th1) - th3)^2)
    V <- nn*(t(grad) %*% S %*% grad)
    se <- sqrt(V/nn)
    return(se)
  }
}

iodnumsq <- function(Y1,FVs1,Y2,FVs2, wt1 = NULL, wt2 = NULL){
  if (is.null(wt1)){
    n1 <- length(Y1)
    n2 <- length(Y2)
    b1 <- 0
    for (i in 1:n1){
      for (j in 1:n2){
        b1 <- b1 + ((FVs1[i] > FVs2[j]) - (FVs2[j] > FVs1[i]))*
          (Y1[i] - Y2[j])
      }
    }
    return(b1)
  }
  else{
    n1 <- length(Y1)
    n2 <- length(Y2)
    b1 <- 0
    for (i in 1:n1){
      for (j in 1:n2){
        b1 <- b1 + wt1[i]*wt2[j]*((FVs1[i] > FVs2[j]) - (FVs2[j] > FVs1[i]))*
          (Y1[i] - Y2[j])
      }
    }
    return(b1)
  }
}

iodnumtr <- function(Y, FVs, wt = NULL){
  if (is.null(wt)){
    n <- length(Y)
    i1 <- n-1
    b1 <- 0
    for (i in 1:i1){
      j1 <- i+1
      for (j in j1:n){
        b1 <- b1 + ((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
          (Y[i] - Y[j])
      }
    }
    return(b1)
  }
  else{
    n <- length(Y)
    b1 <- 0
    n1 <- n-1
    for (i in 1:n1){
      j1 <- i+1
      for (j in j1:n){
        b1 <- b1 + wt[i]*wt[j]*((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
          (Y[i] - Y[j])
      }
    }
    return(b1)
  }
}

peffect_aux <- function(Y,
                        X,
                        FVs,
                        ineq = c("Gini", "MLD",c("Gini","MLD")),
                        ML = c("Lasso","Ridge","RF","CIF","XGB","CB","SL"),
                        ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                        K = 5,
                        iop_gini = NULL,
                        iop_mld = NULL,
                        circ,
                        pe_rel = FALSE,
                        weights = NULL){
  Xk <- dplyr::select(X,-all_of(circ))
  res <- IOD(Y,
             Xk,
             CFit = TRUE,
             npart = K,
             ineq = ineq,
             ML = ML,
             ensemble = ensemble,
             fitted_values = TRUE,
             weights = weights)
  if("Gini" %in% ineq & "MLD" %in% ineq){
    iopk_g <- res$IOp["IOp","Gini"]
    iopk_m <- res$IOp["IOp","MLD"]
    FVsk <- res$FVs
    pe_g <- iop_gini - iopk_g
    pe_m <- iop_mld - iopk_m

    if (pe_rel == TRUE){
      perel_g <- pe_g/iop_gini
      perel_m <- pe_m/iop_mld
    } else{
      perel_g = NULL
      perel_m = NULL
    }
  }
  else if ("Gini" %in% ineq & !"MLD" %in% ineq){
    iopk_g <- res$IOp["IOp","Gini"]
    FVsk <- res$FVs
    pe_g <- iop_gini - iopk_g
    if (pe_rel == TRUE){
      perel_g <- pe_g/iop_gini
    } else{
      perel_g = NULL
    }
  }
  else if (!"Gini" %in% ineq & "MLD" %in% ineq){
    iopk_m <- res$IOp["IOp","MLD"]
    FVsk <- res$FVs
    pe_m <- iop_mld - iopk_m
    if (pe_rel == TRUE){
      perel_m <- pe_m/iop_mld
    } else{
      perel_m = NULL
    }
  }

  if ("Gini" %in% ineq){
    n <- length(Y)
    S11 <- 0
    S22 <- 0
    S12 <- 0
    S <- 0
    if (is.null(weights)){
      for(i in 1:n){
        S11aux <- 0
        S22aux <- 0
        S12aux <- 0
        for (j in 1:n){
          S11aux <- S11aux + (i!=j)*(iop_gini*(Y[i] + Y[j]) -
                                       ((FVs[i] - FVs[j] > 0) - (FVs[i] - FVs[j] < 0))*(Y[i] - Y[j]))
          S22aux <- S22aux + (i!=j)*(iopk_g*(Y[i] + Y[j]) -
                                       ((FVsk[i] - FVsk[j] > 0) - (FVsk[i] - FVsk[j] < 0))*(Y[i] - Y[j]))
        }
        S11 <- S11 + S11aux^2
        S22 <- S22 + S22aux^2
        S12 <- S12 + S11aux*S22aux
      }
      S11 <- (1/(n*((n-1)^2)))*S11
      S22 <- (1/(n*((n-1)^2)))*S22
      S12 <- (1/(n*((n-1)^2)))*S12
      V <- (1/(mean(Y)^2))*(S11 + S22 - 2*S12)
      se_g <- sqrt(V/n)

      if (pe_rel == TRUE){
        V_rel <- (1/(mean(Y)^2))*(((iopk_g^2)/(iop_gini^4))*S11 +
                                    (1/(iop_gini^2))*S22 -
                                    2*(iopk_g/(iop_gini^3))*S12)
        se_rel_g <- sqrt(V_rel/n)
      } else{
        se_rel_g <- NULL
      }
    }
    else{
      WW <- 0
      for(i in 1:n){
        S11aux <- 0
        S22aux <- 0
        S12aux <- 0
        for (j in 1:n){
          S11aux <- S11aux + (i!=j)*(iop_gini*(Y[i] + Y[j]) -
                                       ((FVs[i] - FVs[j] > 0) - (FVs[i] - FVs[j] < 0))*(Y[i] - Y[j]))
          S22aux <- S22aux + (i!=j)*(iopk_g*(Y[i] + Y[j]) -
                                       ((FVsk[i] - FVsk[j] > 0) - (FVsk[i] - FVsk[j] < 0))*(Y[i] - Y[j]))
          WW <- WW + (j>i)*weights[i]*weights[j]
        }
        S11 <- S11 + S11aux^2
        S22 <- S22 + S22aux^2
        S12 <- S12 + S11aux*S22aux
      }
      S11 <- (1/(n*((n-1)^2)))*S11
      S22 <- (1/(n*((n-1)^2)))*S22
      S12 <- (1/(n*((n-1)^2)))*S12
      B = weighted.mean(Y,weights)
      wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
      V <- (1/(B^2))*(S11 + S22 - 2*S12)*sum(wt2^2)
      se_g <- sqrt(V)
      if (pe_rel == TRUE){
        V_rel <- (1/(B^2))*(((iopk_g^2)/(iop_gini^4))*S11 +
                              (1/(iop_gini^2))*S22 -
                              2*(iopk_g/(iop_gini^3))*S12)*sum(wt2^2)
        se_rel_g <- sqrt(V)
      } else{
        se_rel_g <- NULL
      }
    }
  }
  if ("MLD" %in% ineq){
    nn <- length(Y)
    wt <- if (is.null(weights)) rep(1/nn,nn) else weights
    th2 <- weighted.mean(log(FVs),wt)
    th2k <- weighted.mean(log(FVsk),wt)
    S11 <- nn*sum(wt^2*(log(FVs) - th2 + (1/FVs)*(Y - FVs))^2)
    S22 <- nn*sum(wt^2*(log(FVsk) - th2k + (1/FVsk)*(Y - FVsk))^2)
    S12 <- nn*sum(wt^2*(log(FVs) - th2 + (1/FVs)*(Y - FVs))*
                    (log(FVsk) - th2k + (1/FVsk)*(Y - FVsk)))
    V <- S11 + S22 - 2*S12
    se_m <- sqrt(V/nn)

    if (pe_rel == TRUE){
      th1 <- weighted.mean(Y,wt)

      PSI <- wt*matrix(c(Y - th1, log(FVs) - th2 + (1/FVs)*(Y - FVs),
                         log(FVsk) - th2k + (1/FVsk)*(Y - FVsk)),nn,3)
      S <- t(PSI) %*% PSI
      grad <- c((th2 - th2k)/(th1*(log(th1) - th2)^2),
                (th2k - log(th1))/(log(th1) - th2)^2,
                1/(log(th1) - th2))
      V <- nn*(t(grad) %*% S %*% grad)
      se_rel_m <- sqrt(V/nn)
    } else{
      se_rel_m <- NULL
    }
  }
  if ("Gini" %in% ineq & "MLD" %in% ineq){
    res <- rbind(c(pe_g,pe_m),c(se_g,se_m))
    rownames(res) <- c("PE","se")
    colnames(res) <- c("Gini","MLD")
    if (pe_rel == TRUE){
      resrel <- rbind(c(perel_g,perel_m),c(se_rel_g,se_rel_m))
      rownames(resrel) <- c("PE_rel","se")
      colnames(resrel) <- c("Gini","MLD")
    } else{resrel <- NULL}
    return(list(PE = res, PE_rel = resrel))
  }
  else if ("Gini" %in% ineq & !"MLD" %in% ineq){
    res <- rbind(pe_g,se_g)
    rownames(res) <- c("PE","se")
    colnames(res) <- c("Gini")
    if (pe_rel == TRUE){
      resrel <- rbind(perel_g,se_rel_g)
      rownames(resrel) <- c("PE_rel","se")
      colnames(resrel) <- c("Gini")
    } else{resrel <- NULL}
    return(list(PE = res, PE_rel = resrel))
  }
  else if (!"Gini" %in% ineq & "MLD" %in% ineq){
    res <- rbind(pe_m,se_m)
    rownames(res) <- c("PE","se")
    colnames(res) <- c("MLD")
    if (pe_rel == TRUE){
      resrel <- rbind(perel_m,se_rel_m)
      rownames(resrel) <- c("PE_rel","se")
      colnames(resrel) <- c("MLD")
    } else{resrel <- NULL}
    return(list(PE = res, PE_rel = resrel))
  }
}



SP <- function(df, npart){
  nn <- nrow(df)
  p <- base::split(sample(nn,nn,replace = FALSE),as.factor(1:npart))
  # p <- hyperSMURF::do.random.partition(nn, npart, seed = 0)
  dfsp <- NULL
  for (i in 1:npart){
    dfsp[[i]] <- as_tibble(df[p[[i]],])
  }
  return(dfsp)
}

dfnotl <- function(dfcf,i,j){
  aux <- dfcf[-i]
  #If we are not in a triangle
  if (i != j){
    #Observations not in K=i and K=j (i.e. not in I_l)
    aux <- dfcf[-c(i,j)]
    if (length(aux) == 0){
      stop(paste("Dataframe with observations not in fold",i,
                 "and not in fold",print(j),"is empty. Consider using
                        more folds"))
    }
  }
  #ldply applies function to each element of a list and then combines results
  #in a dataframe, if no function is specified it combines everything in a
  #dataframe
  aux <- plyr::ldply(aux)
}

weighted.mean2 <- function(X,weights = NULL){
  if(is.null(weights)){weighted.mean(X)}
  else{weighted.mean(X,weights)}
}

sumUw <- function(w){
  WW <- 0
  n <- length(w)
  n1 <- n - 1
  for (i in 1:n1){
    j1 <- i + 1
    for (j in j1:n){
      WW <- WW + w[i]*w[j]
    }
  }
  return(WW)
}

mld <- function(X, weights = NULL){
  if (is.null(weights) == TRUE){
    return(log(mean(X)) - mean(log(X)))
  }
  else{
    return(log(sum(weights*X)) - sum(weights*log(X)))
  }
}

logliniop <- function(X,
                      Y,
                      ineq = c("Gini", "MLD",c("Gini","MLD")),
                      IOp_rel = FALSE,
                      fitted_values = FALSE,
                      weights = NULL){
  df <- dplyr::as_tibble(cbind(Y = Y, X))
  mll <- stats::lm(log(Y)~.,df, weights = weights)
  FVs <- exp(mll$fitted.values)
  FVs <- FVs*(FVs > 0) + (FVs <= 0)
  if(sum(FVs <= 0) != 0){
    warning(paste(sum(FVs <= 0),"FVs were lower or equal than 0 and were
                    turned into the value 1."))
  }
  res <- sapply(ineq, function(u){
    if (u == "Gini"){
      iopi <- unlist(acid::weighted.gini(FVs, weights)[2])
      if (IOp_rel == TRUE){
        G <- acid::weighted.gini(Y,weights)
        G <- as.numeric(G[2])
        iorel <- iopi/G
        res <- c(iopi, iorel)
        return(res)
      }
      else {return(iopi)}
    }
    else if (u == "MLD"){
      iopi <- mld(FVs, weights)
      if (IOp_rel == TRUE){
        MLD <- mld(Y,weights)
        iorel <- iopi/MLD
        res <- c(iopi, iorel)
        return(res)
      }
      else{return(iopi)}
    }
  })
  if (fitted_values == TRUE){
    return(list(IOp = res, FVs = FVs))
  }
  else{return(res)}
}

npiop <- function(X,
                  Y,
                  ineq = c("Gini", "MLD",c("Gini","MLD")),
                  IOp_rel = FALSE,
                  fitted_values = FALSE,
                  weights = NULL){
  df <- dplyr::as_tibble(cbind(Y = Y,X))
  circs <- colnames(dplyr::select(df,-Y))
  aux <- df %>% dplyr::group_by_at(circs) %>%
                dplyr::mutate(FVs = mean(Y))
  FVs <- aux$FVs
  res <- sapply(ineq, function(u){
    if (u == "Gini"){
      iopi <- unlist(acid::weighted.gini(FVs, weights)[2])
      if (IOp_rel == TRUE){
        G <- acid::weighted.gini(Y,weights)
        G <- as.numeric(G[2])
        iorel <- iopi/G
        res <- c(iopi, iorel)
        return(res)
      }
      else {return(iopi)}
    }
    else if (u == "MLD"){
      iopi <- mld(FVs, weights)
      if (IOp_rel == TRUE){
        MLD <- mld(Y,weights)
        iorel <- iopi/MLD
        res <- c(iopi, iorel)
        return(res)
      }
      else{return(iopi)}
    }
  })
  if (IOp_rel == TRUE){
    rownames(res) <- c("IOp","IOp_rel")
  }
  else {names(res) <- ineq}
  if (fitted_values == TRUE){
    return(list(IOp = res, FVs = FVs))
  }
  else{return(res)}
}

mliop <- function(X,
                  Y,
                  ML = c("Lasso","Ridge","RF","CIF","XGB","CB","SL"),
                  ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                  ineq = c("Gini", "MLD",c("Gini","MLD")),
                  IOp_rel = FALSE,
                  fitted_values = FALSE,
                  weights = NULL){
  ML = match.arg(ML)
  # ineq = match.arg(ineq)
  #Estimate FVs
  m <- ML::MLest(X,
                 Y,
                 ML,
                 ensemble = ensemble,
                 FVs = TRUE,
                 weights = weights)
  FVs <- m$FVs
  FVs <- FVs*(FVs > 0) + (FVs <= 0)
  if(sum(FVs <= 0) != 0){
    warning(paste(sum(FVs <= 0),"FVs were lower or equal than 0 and were
                    turned into the value 1."))
  }
  #Estimate plug in IOp
  res <- sapply(ineq, function(u){
    if (u == "Gini"){
      iopi <- unlist(acid::weighted.gini(FVs, weights)[2])
      if (IOp_rel == TRUE){
        G <- acid::weighted.gini(Y,weights)
        G <- as.numeric(G[2])
        iorel <- iopi/G
        res <- c(iopi, iorel)
        return(res)
      }
      else {return(iopi)}
    }
    else if (u == "MLD"){
      iopi <- mld(FVs, weights)
      if (IOp_rel == TRUE){
        MLD <- mld(Y,weights)
        iorel <- iopi/MLD
        res <- c(iopi, iorel)
        return(res)
      }
      else{return(iopi)}
    }
  })
  if (IOp_rel == TRUE){
    rownames(res) <- c("IOp","IOp_rel")
  }
  else {names(res) <- ineq}
  if (IOp_rel == TRUE){
    rownames(res) <- c("IOp","IOp_rel")
  }
  else {names(res) <- ineq}
  if (fitted_values == TRUE){
    return(list(IOp = res, FVs = FVs))
  }
  else{return(res)}
}
