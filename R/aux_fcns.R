####### AUXILIARY FOR ESTIMATION #############
sepi <- function(Y,FVs,iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq = match.arg(ineq)
  if (ineq == "Gini"){
    if (is.null(weights)){
      n <- length(FVs)
      S <- sapply(1:n, function(u){
        a <- sum((1/(n-1))*(iop*(FVs[u] + FVs[-u]) - abs(FVs[u] - FVs[-u])))
      })
      S <- sum(S^2)
      B = 2*mean(Y)
      S = (4/n)*S
      V = (1/B^2)*S
      se = sqrt(V/n)
      return(se)
    }
    else{
      n <- length(FVs)
      SS <- lapply(1:n, function(u){
        a <- sum((1/(n-1))*(iop*(FVs[u] + FVs[-u]) - abs(FVs[u] - FVs[-u])))
        u1 <- u + 1
        if (u != n){
          b <-  sum(weights[u]*weights[u1:n])
        }
        else{
          b <- 0
        }
        data.frame(S = a, WW = b)
      })
      SS <- do.call(rbind,SS)
      S <- sum((1/n)*SS$S^2)
      WW <- sum(SS$WW)
      wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
      S = 4*S*sum(wt2^2)

      B = 2*weighted.mean(Y,weights)
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

# io_deb <- function(Y, FVs, ineq = c("Gini", "MLD"), weights = NULL){
#   ineq = match.arg(ineq)
#   if (ineq == "Gini"){
#     if (is.null(weights)){
#       n <- length(Y)
#       i1 <- n-1
#       b1 <- 0
#       for (i in 1:i1){
#         j1 <- i+1
#         for (j in j1:n){
#           b1 <- b1 + ((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
#             (Y[i] - Y[j])
#         }
#       }
#       return(b1/((n-1)*n*mean(Y)))
#     }
#     else{
#       n <- length(Y)
#       b1 <- 0
#       WW <- 0
#       n1 <- n-1
#       for (i in 1:n1){
#         j1 <- i+1
#         for (j in j1:n){
#           b1 <- b1 + weights[i]*weights[j]*((FVs[i] > FVs[j]) - (FVs[j] > FVs[i]))*
#             (Y[i] - Y[j])
#           WW <- WW + weights[i]*weights[j]
#         }
#       }
#       den <- WW*2*weighted.mean(Y,wt)
#       return(b1/den)
#     }
#   }
#   else if (ineq == "MLD"){
#     wt <- if (is.null(weights)) rep(1/length(Y),length(Y)) else weights
#     th1 <- weighted.mean(Y,wt)
#     th2 <- weighted.mean(log(FVs) + (1/FVs)*(Y-FVs), wt)
#     return(log(th1) - th2)
#   }
# }


io_deb <- function(Y, FVs, ineq = c("Gini", "MLD"), weights = NULL){
  ineq = match.arg(ineq)
  if (ineq == "Gini"){
    if (is.null(weights)){weights = rep(1,length(Y))}
    wt <- weights/sum(weights)
    n <- length(Y)
    aa <- dplyr::arrange(data.frame("Y" = Y, "FVs" = FVs, "wt"= wt),FVs)
    aa$R <- cumsum(aa$wt)/sum(aa$wt)
    covyfv <- weighted.mean(aa$Y*aa$R, aa$wt) -
      weighted.mean(aa$Y,aa$wt)*weighted.mean(aa$R,aa$wt)
    return(2*covyfv/weighted.mean(Y,wt))
  }
  else if (ineq == "MLD"){
    wt <- if (is.null(weights)) rep(1/length(Y),length(Y)) else weights
    th1 <- weighted.mean(Y,wt)
    th2 <- weighted.mean(log(FVs) + (1/FVs)*(Y-FVs), wt)
    return(log(th1) - th2)
  }
}

se_deb2 <- function(Y, FVs, iop, weights = NULL){
  n <- length(Y)
  if (is.null(weights)){
    d <- sapply(1:n, function(u){
      Y_FVs_i <- cbind(Y,FVs)[u,]
      d <- mean(sign(Y_FVs_i[2] - FVs)*(Y_FVs_i[1] - Y))
    })
    V <- 4*var(iop*Y - d)/((2*mean(Y))^2)
    return(sqrt(V)/sqrt(length(Y)))
  }
  else{
    d <- sapply(1:n, function(u){
      Y_FVs_i <- cbind(Y,FVs)[u,]
      d <- weighted.mean(sign(Y_FVs_i[2] - FVs)*(Y_FVs_i[1] - Y), weights)
    })
    VV <- weighted.mean((iop*Y - d)^2,weights) - weighted.mean(iop*Y - d,weights)^2
    V <- 4*VV/((2*weighted.mean(Y,weights))^2)
    return(sqrt(V)/sqrt(length(Y)))
  }
}

se_deb3 <- function(Y, FVs, iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq <- match.arg(ineq)
  if (ineq == "Gini"){
    se <- se_deb2(Y = Y, FVs = FVs, iop = iop, weights = weights)
  }
  else if (ineq == "MLD"){
    se <- se_deb(Y = Y, FVs = FVs, iop = iop, ineq = ineq, weights = weights)
  }
  return(se)
}

se_deb <- function(Y, FVs, iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq <- match.arg(ineq)
  if (ineq == "Gini"){
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

se_deb_alt <- function(Y, FVs, alpha, iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq <- match.arg(ineq)
  if (ineq == "Gini"){
    n <- length(Y)
    if(is.null(weights)){
      aux <- sapply(1:n, function(u){
        a <- sum((1/(n-1))*(iop*(Y[u] + Y[-u]) -
                              abs(FVs[u] - FVs[-u]) - alpha[u]*(Y[u] - FVs[u]) -
                              alpha[-u]*(Y[-u] - FVs[-u])))
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
                               abs(FVs[u] - FVs[-u]) - alpha[u]*(Y[u] - FVs[u]) -
                               alpha[-u]*(Y[-u] - FVs[-u])))
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

se_deb_unb <- function(Y, FVs, iop, ineq = c("Gini", "MLD"), weights = NULL){
  ineq <- match.arg(ineq)
  if (ineq == "Gini"){
    n <- length(Y)
    if(is.null(weights)){
      aux <- sapply(1:n, function(u){
        a <- sum((iop*(Y[u] + Y[-u]) -
                    ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                    (Y[u] - Y[-u])))
        b = sum((iop*(Y[u] + Y[-u]) -
                   ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                   (Y[u] - Y[-u]))^2)
      })
      C1 <- sum(aux[1,]^2)
      C2 = sum(aux[2,])
      Un = (1/(n*(n-1)))*sum(aux[1,])
      n4m = n*(n-1)*(n-2)
      VU = (4*C1 - 2*C2)/n4m - (4*n-6)/((n-2)*(n-3))*Un^2
      VU = VU/((2*mean(Y))^2)
      seU = sqrt(VU/n)
      return(seU)
    }
    else{
      aux <- sapply(1:n, function(u){
        a <- sum((iop*(Y[u] + Y[-u]) -
                    ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                    (Y[u] - Y[-u])))
        b = sum((iop*(Y[u] + Y[-u]) -
                   ((FVs[u] > FVs[-u]) - (FVs[-u] > FVs[u]))*
                   (Y[u] - Y[-u]))^2)
        if (u!=n){
          u1 <- u + 1
          WW <- sum(weights[u]*weights[u1:n])
        }
        else{
          WW <- 0
        }
        return(c(a,b,WW))
      })
      # aux <- do.call(rbind, aux)
      C1 <- sum(aux[1,]^2)
      C2 = sum(aux[2,])
      Un = (1/(n*(n-1)))*sum(aux[1,])
      WW <- sum(aux[3,])
      n4m = n*(n-1)*(n-2)
      VU = (4*C1 - 2*C2)/n4m - (4*n-6)/((n-2)*(n-3))*Un^2
      wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
      vnum = VU*sum(wt2^2)
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

# iodnumsq <- function(Y1,FVs1,Y2,FVs2, wt1 = NULL, wt2 = NULL){
#   if (is.null(wt1)){
#     n1 <- length(Y1)
#     a <- lapply(1:n1, function(u){
#       sum(((FVs1[u] > FVs2) - (FVs2 > FVs1[u]))*
#             (Y1[u] - Y2))
#     })
#     b1 <- sum(a)
#     return(b1)
#   }
#   else{
#     n1 <- length(Y1)
#     a <- sapply(1:n1, function(u){
#       sum(wt1[u]*wt2*((FVs1[u] > FVs2) - (FVs2 > FVs1[u]))*
#             (Y1[u] - Y2))
#     })
#     b1 <- sum(a)
#     return(b1)
#   }
# }

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

iodnumsq_alt <- function(Y1,FVs1,Y2,FVs2, a1, a2, wt1 = NULL, wt2 = NULL){
  if (is.null(wt1)){
    n1 <- length(Y1)
    a <- sapply(1:n1, function(u){
      sum(abs(FVs1[u] - FVs2) + a1[u]*(Y1[u] - FVs1[u]) + a2*(Y2 - FVs2))
    })
    b1 <- sum(a)
    return(b1)
  }
  else{
    n1 <- length(Y1)
    a <- sapply(1:n1, function(u){
      sum(wt1[u]*wt2*(abs(FVs1[u] - FVs2) + a1[u]*(Y1[u] - FVs1[u]) + a2*(Y2 - FVs2)))
    })
    b1 <- sum(a)
    return(b1)
  }
}



iodnumtr <- function(Y, FVs, wt = NULL){
  if (is.null(wt)){
    n <- length(Y)
    i1 <- n-1
    a <- sapply(1:i1, function(u){
      j1 <- u + 1
      sum(((FVs[u] > FVs[j1:n]) - (FVs[j1:n] > FVs[u]))*
            (Y[u] - Y[j1:n]))
    })
    b1 <- sum(a)
    return(b1)
  }
  else{
    n <- length(Y)
    i1 <- n-1
    a <- sapply(1:i1, function(u){
      j1 <- u + 1
      sum(wt[u]*wt[j1:n]*((FVs[u] > FVs[j1:n]) - (FVs[j1:n] > FVs[u]))*
            (Y[u] - Y[j1:n]))
    })
    b1 <- sum(a)
    return(b1)
  }
}

# iodnumtr_new <- function(Y1, Y2, FVs1, FVs2, wt1 = NULL, wt2 = NULL){
#   if (is.null(wt1)){
#     n1 <- length(Y1)
#     n2 = length(Y2)
#     a <- sapply(1:n1, function(u){
#       sum(((FVs1[u] > FVs2[u:n2]) - (FVs2[u:n2] > FVs1[u]))*
#             (Y1[u] - Y2[u:n2]))
#     })
#     b1 <- sum(a)
#     return(b1)
#   }
#   else{
#     n1 <- length(Y1)
#     n2 <- length(Y2)
#     a <- sapply(1:n1, function(u){
#       sum(wt1[u]*wt2[u:n2]*((FVs1[u] > FVs2[u:n2]) - (FVs2[u:n2] > FVs1[u]))*
#             (Y1[u] - Y2[u:n2]))
#     })
#     b1 <- sum(a)
#     return(b1)
#   }
# }

iodnumtr_new <- function(Y1, Y2, FVs1, FVs2, wt1 = NULL, wt2 = NULL, FVs01, FVs02){
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

iodnumtr_new_alt <- function(Y1, Y2, FVs1, FVs2, a1, a2, wt1 = NULL, wt2 = NULL){
  if (is.null(wt1)){
    n1 <- length(Y1)
    n2 = length(Y2)
    a <- sapply(1:n1, function(u){
      sum(abs(FVs1[u] - FVs2[u:n2]) + a1[u]*(Y1[u] - FVs1[u]) + a2[u:n2]*(Y2[u:n2] - FVs2[u:n2]))
    })
    b1 <- sum(a)
    return(b1)
  }
  else{
    n1 <- length(Y1)
    n2 <- length(Y2)
    a <- sapply(1:n1, function(u){
      sum(wt1[u]*wt2[u:n2]*
            (abs(FVs1[u] - FVs2[u:n2]) + a1[u]*(Y1[u] - FVs1[u]) + a2[u:n2]*(Y2[u:n2] - FVs2[u:n2])))
    })
    b1 <- sum(a)
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
  if (ML == "Lasso" | ML == "Ridge"){
    Xk <- dplyr::select(as_tibble(X),-dplyr::starts_with(circ))
  }
  else{
    Xk <- dplyr::select(X,-all_of(circ))
  }

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
      a <- lapply(1:n, function(u){
        S11aux <- sum((1/(n-1))*(iop_gini*(Y[u] + Y[-u]) -
                                   ((FVs[u] - FVs[-u] > 0) - (FVs[u] - FVs[-u] < 0))*(Y[u] - Y[-u])))
        S22aux <- sum((1/(n-1))*(iopk_g*(Y[u] + Y[-u]) -
                                   ((FVsk[u] - FVsk[-u] > 0) - (FVsk[u] - FVsk[-u] < 0))*(Y[u] - Y[-u])))
        data.frame(S11aux = S11aux, S22aux = S22aux)
      })
      a <- do.call(rbind,a)
      S11 <- sum((1/n)*a$S11aux^2)
      S22 <- sum((1/n)*a$S22aux^2)
      S12 <- sum((1/n)*a$S11aux*a$S22aux)
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
      a <- lapply(1:n, function(u){
        S11aux <- sum((1/(n-1))*(iop_gini*(Y[u] + Y[-u]) -
                                   ((FVs[u] - FVs[-u] > 0) - (FVs[u] - FVs[-u] < 0))*(Y[u] - Y[-u])))
        S22aux <- sum((1/(n-1))*(iopk_g*(Y[u] + Y[-u]) -
                                   ((FVsk[u] - FVsk[-u] > 0) - (FVsk[u] - FVsk[-u] < 0))*(Y[u] - Y[-u])))
        if(u != n){
          u1 <- u + 1
          WW <- sum(weights[u]*weights[u1:n])
        }
        else{
          WW <- 0
        }
        data.frame(S11aux = S11aux, S22aux = S22aux, WW = WW)
      })
      a <- do.call(rbind,a)
      S11 <- sum((1/n)*a$S11aux^2)
      S22 <- sum((1/n)*a$S22aux^2)
      S12 <- sum((1/n)*a$S11aux*a$S22aux)
      WW <- sum(a$WW)
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
  dfsp <- NULL
  for (i in 1:npart){
    dfsp[[i]] <- dplyr::as_tibble(df[p[[i]],])
  }
  return(list(dfsp = dfsp, indices = p))
}

SP_new <- function(df){
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
    dfcfi[[kk]] = data.frame(df[cf.ilist[[kk]],], cf.ilist[[kk]])
    dfcfj[[kk]] = data.frame(df[cf.jlist[[kk]],], cf.jlist[[kk]])
  }
  # Arrange stuff for plotting CF
  # blcks = c("I1","I2","I3","I4","I5","I6")
  # aux = NULL
  # for (kk in 1:6){
  #   if (kk %in% c(1,2)){
  #     ncf1 = length(cf.ilist[[kk]])
  #     ncf2 = length(cf.jlist[[kk]])
  #
  #     pldf = data.frame(ci = rep(NA,ncf1*(ncf2+1)*0.5),
  #                       cj = rep(NA,ncf1*(ncf2+1)*0.5),
  #                       Il = rep(NA,ncf1*(ncf2+1)*0.5))
  #     cnt = 0
  #     for (ii in 1:ncf1){
  #       jj1 = ii
  #       for (jj in jj1:ncf2){
  #         cnt = cnt + 1
  #         pldf[cnt,] = c(cf.ilist[[kk]][ii], cf.jlist[[kk]][jj],blcks[kk])
  #       }
  #     }
  #     aux[[kk]] = pldf
  #   } else{
  #     ncf1 = length(cf.ilist[[kk]])
  #     ncf2 = length(cf.jlist[[kk]])
  #
  #     pldf = data.frame(ci = rep(NA,ncf1*ncf2),
  #                       cj = rep(NA,ncf1*ncf2),
  #                       Il = rep(NA,ncf1*ncf2))
  #     cnt = 0
  #     for (ii in 1:ncf1){
  #       for (jj in 1:ncf2){
  #         cnt = cnt + 1
  #         pldf[cnt,] = c(cf.ilist[[kk]][ii], cf.jlist[[kk]][jj],blcks[kk])
  #       }
  #     }
  #     aux[[kk]] = pldf
  #   }
  # }
  # pldf = do.call(rbind,aux)
  # pldf$ci = as.numeric(pldf$ci)
  # pldf$cj = as.numeric(pldf$cj)
  #
  # ggplot(pldf,aes(ci,cj, colour = Il)) +
  #   geom_point()
  # scale_x_discrete(name ="i",
  #                  limits=factor(1:nn)) +
  # scale_y_discrete(name ="j",
  #                  limits=factor(1:nn)) +
  # geom_abline(intercept = 0, slope = 1)

  return(list(dfcfi = dfcfi, dfcfj = dfcfj))
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

dfnotl_new <- function(df,dfcfi,dfcfj,k){
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

sumUw2 <- function(w){
  WW <- 0
  n <- length(w)
  for (i in 1:n){
    for (j in 1:n){
      WW <- WW + w[i]*(i!=j)
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
                  ML = c("Lasso","Ridge","RF","CIF","XGB","CB",
                         "OLSensemble", "SL"),
                  OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                  SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                  ensemblefolds = 5,
                  ineq = c("Gini", "MLD",c("Gini","MLD")),
                  IOp_rel = FALSE,
                  fitted_values = FALSE,
                  rf.cf.ntree = 500,
                  rf.depth = NULL,
                  mtry = max(floor(ncol(X)/3), 1),
                  polynomial.Lasso = 1,
                  polynomial.Ridge = 1,
                  xgb.nrounds = 200,
                  xgb.max.depth = 6,
                  cb.iterations = 1000,
                  cb.depth = 6,
                  weights = NULL,
                  extFVs = NULL){
  ML = match.arg(ML)
  # ineq = match.arg(ineq)
  #Estimate FVs
  if (is.null(extFVs)){
    m <- ML::MLest(X,
                   Y,
                   ML,
                   OLSensemble = ensemble,
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
    FVs <- m$FVs
  }
  if (!is.null(extFVs)){
    FVs <- extFVs
  }

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
      browser()
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
