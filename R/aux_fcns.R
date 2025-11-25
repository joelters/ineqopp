####### AUXILIARY FUNCTIONS #############


io_deb <- function(Y, FVs, weights = NULL){
    if (is.null(weights)){weights = rep(1,length(Y))}
    wt <- weights/sum(weights)
    n <- length(Y)
    aa <- dplyr::arrange(data.frame("Y" = Y, "FVs" = FVs, "wt"= wt),FVs)
    aa$R <- cumsum(aa$wt)/sum(aa$wt)
    covyfv <- weighted.mean(aa$Y*aa$R, aa$wt) -
      weighted.mean(aa$Y,aa$wt)*weighted.mean(aa$R,aa$wt)
    return(2*covyfv/weighted.mean(Y,wt))
}

se_deb <- function(Y, FVs, iop, weights = NULL){
  n <- length(Y)
  if(is.null(weights)){
    aux <- sapply(1:n, function(u){
      a <- sum((1/(n-1))*(iop*(Y[u] + Y[-u]) -
                            ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                            (Y[u] - Y[-u])))
    })
    S <- (1/n)*sum(aux^2)
    S = var(aux)
    vnum = 4*S
    V <- vnum/((2*mean(Y))^2)
    return(sqrt(V)/sqrt(length(Y)))
  }
  else{
    aux <- lapply(1:n, function(u){
      jt <- sum((1/(n-1))*(iop*(Y[u] + Y[-u]) -
                             ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                             (Y[u] - Y[-u])))
      if (u!=n){
        u1 <- u + 1
        WW <- sum(weights[u]*weights[u1:n])
      }
      else{
        WW <- 0
      }
      data.frame(S = jt, WW = WW)
    })
    aux <- do.call(rbind, aux)
    S <- sum((1/n)*aux$S^2)
    WW <- sum(aux$WW)
    vnum <- 4*S
    wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
    vnum = vnum*sum(wt2^2)
    vden <- (2*weighted.mean(Y,weights))^2
    V <- vnum/vden
    return(sqrt(V))
  }
}

se_rel <- function(Y,FVs,iodeb, IY, weights = NULL){
  n <- length(Y)
  S11 <- 0
  S22 <- 0
  S12 <- 0
  S <- 0

  if (is.null(weights)){
    SS <- lapply(1:n, function(u){
      S11aux <- sum((1/(n-1))*(iodeb*(Y[u] + Y[-u]) -
                                 ((FVs[u] - FVs[-u] > 0) - (FVs[u] - FVs[-u] < 0))*(Y[u] - Y[-u])))
      S22aux <- sum((1/(n-1))*(IY*(Y[u] + Y[-u]) - abs(Y[u] - Y[-u])))
      data.frame(S11aux = S11aux, S22aux = S22aux)
    })
    SS <- do.call(rbind,SS)
    S11 <- sum((1/n)*SS$S11aux^2)
    S22 <- sum((1/n)*SS$S22aux^2)
    S12 <- sum((1/n)*SS$S11aux*SS$S22aux)
    V <- (1/(mean(Y)^2))*((S11/(IY^2)) + ((iodeb^2)/(IY^4))*S22 - 2*((iodeb)/(IY^3))*S12)
    se <- sqrt(V/n)
  }
  else{
    SS <- lapply(1:n, function(u){
      S11aux <- sum((1/(n-1))*(iodeb*(Y[u] + Y[-u]) -
                                 ((FVs[u] - FVs[-u] > 0) - (FVs[u] - FVs[-u] < 0))*(Y[u] - Y[-u])))
      S22aux <- sum((1/(n-1))*(IY*(Y[u] + Y[-u]) - abs(Y[u] - Y[-u])))
      if (u!=n){
        u1 <- u + 1
        WWaux <- sum(weights[u]*weights[u1:n])
      }
      else{
        WWaux <- 0
      }
      data.frame(S11aux = S11aux, S22aux = S22aux, WWaux = WWaux)
    })
    SS <- do.call(rbind,SS)
    S11 <- sum((1/n)*SS$S11aux^2)
    S22 <- sum((1/n)*SS$S22aux^2)
    S12 <- sum((1/n)*SS$S11aux*SS$S22aux)
    WW <- sum(SS$WWaux)
    B = weighted.mean(Y,weights)
    wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
    V <- (1/(B^2))*((S11/(IY^2)) + ((iodeb^2)/(IY^4))*S22 - 2*((iodeb)/(IY^3))*S12)*sum(wt2^2)
    se <- sqrt(V)
  }
  se
}

iodnumsq <- function(Y1,FVs1,Y2,FVs2, wt1 = NULL, wt2 = NULL, FVs01, FVs02){
  if (is.null(wt1)){
    n1 <- length(Y1)
    a <- lapply(1:n1, function(u){
      a1 <- sum(((FVs1[u] > FVs2) - (FVs2 > FVs1[u]))*
                  (Y1[u] - Y2))
      a2 <- (((FVs1[u] > FVs2) - (FVs2 > FVs1[u])) !=
               ((FVs01[u] > FVs02) - (FVs02 > FVs01[u])))
      list(a1 = a1, a2 = a2)
    })
    b1 <- sum(sapply(a, function(x) x$a1))
    sgns <- unlist(lapply(a, function(x) x$a2))
    return(list(b1 = b1, sgns = sgns))
  }
  else{
    n1 <- length(Y1)
    a <- lapply(1:n1, function(u){
      a1 <- sum(wt1[u]*wt2*((FVs1[u] > FVs2) - (FVs2 > FVs1[u]))*
                  (Y1[u] - Y2))
      a2 <- (((FVs1[u] > FVs2) - (FVs2 > FVs1[u])) !=
               ((FVs01[u] > FVs02) - (FVs02 > FVs01[u])))
      list(a1 = a1, a2 = a2)
    })
    b1 <- sum(sapply(a, function(x) x$a1))
    sgns <- unlist(lapply(a, function(x) x$a2))
    return(list(b1 = b1, sgns = sgns))
  }
}

iodnumtr <- function(Y1, Y2, FVs1, FVs2, wt1 = NULL, wt2 = NULL, FVs01, FVs02){
  if (is.null(wt1)){
    n1 <- length(Y1)
    n2 = length(Y2)
    a <- lapply(1:n1, function(u){
      a1 <- sum(((FVs1[u] > FVs2[u:n2]) - (FVs2[u:n2] > FVs1[u]))*
            (Y1[u] - Y2[u:n2]))
      a2 <- (((FVs1[u] > FVs2[u:n2]) - (FVs2[u:n2] > FVs1[u])) !=
        ((FVs01[u] > FVs02[u:n2]) - (FVs02[u:n2] > FVs01[u])))
      list(a1 = a1, a2 = a2)
    })
    b1 <- sum(sapply(a, function(x) x$a1))
    sgns <- unlist(lapply(a, function(x) x$a2))
    return(list(b1 = b1, sgns = sgns))
  }
  else{
    n1 <- length(Y1)
    n2 <- length(Y2)
    a <- lapply(1:n1, function(u){
      a1 <- sum(wt1[u]*wt2[u:n2]*((FVs1[u] > FVs2[u:n2]) - (FVs2[u:n2] > FVs1[u]))*
            (Y1[u] - Y2[u:n2]))
      a2 <- (((FVs1[u] > FVs2[u:n2]) - (FVs2[u:n2] > FVs1[u])) !=
               ((FVs01[u] > FVs02[u:n2]) - (FVs02[u:n2] > FVs01[u])))
      list(a1 = a1, a2 = a2)
    })
    b1 <- sum(sapply(a, function(x) x$a1))
    sgns <- unlist(lapply(a, function(x) x$a2))
    return(list(b1 = b1, sgns = sgns))
  }
}

SP <- function(df){
  nn <- nrow(df)
  # n even and floor(n/2) even, i.e. n divisible by 4
  if (nn %% 4 == 0){
    case.cf = 1
    n2 = nn/2
    n4 = nn/4
    cf.ilist = list(ci1 = 1:(n2-1), ci2 = (n2+1):(nn-1),
                    ci3 = 1:n4, ci4 = (n4 + 1):n2,
                    ci5 = (n4 + 1):n2, ci6 = 1:n4)
    cf.jlist = list(cj1 = 2:n2, cj2 = (n2+2):nn,
                    cj3 = (n2+1):(n2+n4), cj4 = (n2+1):(n2+n4),
                    cj5 = (n2 + n4 + 1):nn, cj6 = (n2 + n4 + 1):nn)
  } else if (nn %% 2 == 0 & floor(nn/2) %% 2 != 0){ # n even and floor(n/2) odd
    case.cf = 2
    n2 = nn/2
    cf.ilist = list(ci1 = 1:(n2-1), ci2 = n2:(nn-1),
                    ci3 = 1:(n2-1-(n2-1)*0.5), ci4 = (n2-(n2-1)*0.5):(n2-1),
                    ci5 = (n2-(n2-1)*0.5):(n2-1), ci6 = 1:(n2-1-(n2-1)*0.5))
    cf.jlist = list(cj1 = 2:(n2+1), cj2 = (n2+1):nn,
                    cj3 = (n2+2):(n2+1+(n2-1)*0.5), cj4 = (n2+2):(n2+1+(n2-1)*0.5),
                    cj5 = (n2+2+(n2-1)*0.5):nn, cj6 = (n2+2+(n2-1)*0.5):nn)
  } else if (nn %% 2 != 0 & floor(nn/2) %% 2 == 0){ # n odd and floor(n/2) even
    case.cf = 3
    n2 = floor(nn/2)
    cf.ilist = list(ci1 = 1:n2, ci2 = (n2+1):(nn-1),
                    ci3 = 1:(n2/2), ci4 = (n2/2 + 1):n2,
                    ci5 = (n2/2 + 1):n2, ci6 = 1:(n2/2))
    cf.jlist = list(cj1 = 2:(n2+1), cj2 = (n2+2):nn,
                    cj3 = (n2+2):(n2+1+n2*0.5), cj4 = (n2+2):(n2+1+n2*0.5),
                    cj5 = (n2+2+n2*0.5):nn, cj6 = (n2+2+n2*0.5):nn)
  } else if (nn %% 2 != 0 & floor(nn/2) %% 2 != 0){ # n odd and floor(n/2) odd
    case.cf = 4
    n2 = floor(nn/2)
    cf.ilist = list(ci1 = 1:(n2-1), ci2 = n2:(nn-1),
                    ci3 = 1:((n2-1)*0.5), ci4 = (n2-(n2-1)*0.5):(n2-1),
                    ci5 = (n2-(n2-1)*0.5):(n2-1), ci6 = 1:((n2-1)*0.5))
    cf.jlist = list(cj1 = 2:(n2+2), cj2 = (n2+1):nn,
                    cj3 = (n2+3):(n2+2+(n2-1)*0.5), cj4 = (n2+3):(n2+2+(n2-1)*0.5),
                    cj5 = (n2+3+(n2-1)*0.5):nn, cj6 = (n2+3+(n2-1)*0.5):nn)
  }

  dfcfi = NULL
  dfcfj = NULL
  for (kk in 1:6){
    dfcfi[[kk]] = data.frame(df[cf.ilist[[kk]],], ind = cf.ilist[[kk]])
    dfcfj[[kk]] = data.frame(df[cf.jlist[[kk]],], ind = cf.jlist[[kk]])
  }
  return(list(dfcfi = dfcfi, dfcfj = dfcfj))
}

dfnotl <- function(df,dfcfi,dfcfj,k){
  ind = c(dfcfi[[k]][,ncol(dfcfi[[k]])],dfcfj[[k]][,ncol(dfcfi[[k]])])
  ind = unique(ind)
  res = df[-ind,]
}

weighted.mean2 <- function(X,weights = NULL){
  if(is.null(weights)){weighted.mean(X)}
  else{weighted.mean(X,weights)}
}

sumUw <- function(w){
  WW <- 0
  n <- length(w)
  n1 <- n - 1
  WW <- sapply(1:n1, function(u){
    j1 <- u + 1
    sum(w[u]*w[j1:n])
  })
  WW <- sum(WW)
  return(WW)
}

mliop_pi <- function(X,
                  Y,
                  ML = c("Lasso","Ridge","RF","CIF","XGB","CB","Torch",
                        "loglin", "NLLS_exp",  "OLSensemble", "SL"),
                  OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                  SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                  ensemblefolds = 5,
                  IOp_rel = FALSE,
                  fitted_values = FALSE,
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
                  weights = NULL,
                  extFVs = NULL){
  ML = match.arg(ML)
  #Estimate FVs
  if (is.null(extFVs)){
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
                   start_nlls = start_nlls,
                   FVs = TRUE,
                   weights = weights)
    FVs <- m$FVs
  }
  if (!is.null(extFVs)){
    FVs <- extFVs
  }

  if(sum(FVs <= 0) != 0){
    warning(paste(sum(FVs <= 0),"FVs are lower or equal than 0."))
  }
  #Estimate plug in IOp
  iopi <- unlist(acid::weighted.gini(FVs, weights)[2])
  if (IOp_rel == TRUE){
    G <- acid::weighted.gini(Y,weights)
    G <- as.numeric(G[2])
    iorel <- iopi/G
    res <- c(iopi, iorel)
  }
  else {res = iopi}

  if (IOp_rel == TRUE){
    names(res) <- c("IOp","IOp_rel")
  } else{names(res) = "IOp"}

  if (fitted_values == TRUE){
    return(list(IOp = res, FVs = FVs))
  }
  else{return(res)}
}

se_PI <- function(Y,FVs,iop, weights = NULL){
  if (is.null(weights)){
    n <- length(FVs)
    S <- sapply(1:n, function(u){
      a <- sum((1/(n-1))*(iop*(Y[u] + Y[-u]) -
                            ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                            (Y[u] - Y[-u])))
      b <- sum((1/(n-1))*(iop*(FVs[u] + FVs[-u]) - abs(FVs[u] - FVs[-u])))
      return(c(a,b))
    })
    S_naive = S[2,]
    S_naive <- sum(S_naive^2)
    B = 2*mean(Y)
    S_naive = (4/n)*S_naive
    V = (1/B^2)*S_naive
    se_naive = sqrt(V/n)

    S = S[1,]
    S <- sum(S^2)
    B = 2*mean(Y)
    S = (4/n)*S
    V = (1/B^2)*S
    se = sqrt(V/n)
    return(data.frame(se = se, se_naive = se_naive))
  }
  else{
    n <- length(FVs)
    SS <- lapply(1:n, function(u){
      a1 = sum((1/(n-1))*(iop*(Y[u] + Y[-u]) -
                            ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                            (Y[u] - Y[-u])))
      a2 <- sum((1/(n-1))*(iop*(FVs[u] + FVs[-u]) - abs(FVs[u] - FVs[-u])))
      u1 <- u + 1
      if (u != n){
        b <-  sum(weights[u]*weights[u1:n])
      }
      else{
        b <- 0
      }
      data.frame(S = a1, S_naive = a2, WW = b)
    })
    SS <- do.call(rbind,SS)

    S <- sum((1/n)*SS$S^2)
    WW <- sum(SS$WW)
    wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
    S = 4*S*sum(wt2^2)
    B = 2*weighted.mean(Y,weights)
    V = (1/B^2)*S
    se = sqrt(V)

    S <- sum((1/n)*SS$S_naive^2)
    WW <- sum(SS$WW)
    wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
    S = 4*S*sum(wt2^2)
    B = 2*weighted.mean(Y,weights)
    V = (1/B^2)*S
    se_naive = sqrt(V)
    return(data.frame(se = se, se_naive = se_naive))
  }
}
