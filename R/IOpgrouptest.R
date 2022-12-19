#' Tests for significance of a group of circumstances
#'
#' `IOpgrouptest` takes the output of IOp estimation and a selection of
#' circumstances for which it performs a joint test.
#'
#' @param Y is a vector containing the (continuous) outcome of interest
#' @param X is a dataframe containing all the circumstances
#' @param circs vector of circumstances for which we want to perform the group test
#' @param FVs fitted values taken from running the function IOp first
#' @param ineq which inequality index among Gini and MLD should be chosen.
#' We can choose to use both.
#' @param ML is a string specifying which machine learner to use, usually one uses
#' the same ML as the one used in the estimation of IOp
#' @param ensemble is a string vector specifying which learners
#' should be used in the SuperLearner
#' @param K in how many parts should the data be split for cross-fitting
#' @param iop_gini an IOp estimate based on the Gini using the full set of circumstances
#' @param iop_mld an IOp estimate based on the MLD using the full set of circumstances
#' @param weights survey weights adding up to 1
#' @returns test results
#' @examples
#' X <- dplyr::select(mad2019,-Y)
#' Y <- mad2019$Y
#' circs <- c("educM","educF")
#' iop <- IOp(Y,
#'            X,
#'            est_method = "Debiased",
#'            CFit = TRUE,
#'            ineq = c("Gini","MLD"),
#'            plugin_method = c("ML"),
#'            ML = "RF",
#'            sterr = FALSE,
#'            fitted_values = TRUE)
#' FVs <- iop$FVs
#' iop_gini <- iop$IOp["IOp","Gini"]
#' iop_mld <- iop$IOp["IOp","MLD"]
#'
#' Gtest <- IOpgrouptest(Y,
#'                       X,
#'                       FVs = FVs,
#'                       ineq = c("Gini","MLD"),
#'                       ML = "RF",
#'                       K = 5,
#'                       iop_gini = iop_gini,
#'                       iop_mld = iop_mld,
#'                       circs = circs)
#' Gtest$Gini
#' Gtest$MLD
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#'
#' Terschuur, J. (2022). Debiased Machine Learning Inequality
#' of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
#' @export
IOpgrouptest <- function(Y,
                         X,
                         FVs,
                         ineq = c("Gini", "MLD",c("Gini","MLD")),
                         ML = c("Lasso","Ridge","RF","CIF","XGB","CB","SL"),
                         ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                         K = 5,
                         iop_gini = NULL,
                         iop_mld = NULL,
                         circs,
                         weights = NULL){
  Xk <- dplyr::select(X,-all_of(circs))
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
  }
  else if ("Gini" %in% ineq & !"MLD" %in% ineq){
    iopk_g <- res$IOp["IOp","Gini"]
    FVsk <- res$FVs
    pe_g <- iop_gini - iopk_g
  }
  else if (!"Gini" %in% ineq & "MLD" %in% ineq){
    iopk_m <- res$IOp["IOp","MLD"]
    FVsk <- res$FVs
    pe_m <- iop_mld - iopk_m
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
  }
  if ("Gini" %in% ineq & "MLD" %in% ineq){
    tstatg <- pe_g/se_g
    pvalg <- 2*(pnorm(1 - abs(tstatg)))
    resg <- c(difference = pe_g, se = se_g, t.stat = tstatg,
              pvalue = pvalg)
    tstatm <- pe_m/se_m
    pvalm <- 2*(pnorm(1 - abs(tstatm)))
    resm <- c(difference = pe_m, se = se_m, t.stat = tstatm,
              pvalue = pvalm)
    return(list(Gini = resg, MLD = resm))
  }
  else if ("Gini" %in% ineq & !"MLD" %in% ineq){
    tstatg <- pe_g/se_g
    pvalg <- 2*(pnorm(1 - abs(tstatg)))
    resg <- c(difference = pe_g, se = se_g, t.stat = tstatg,
              pvalue = pvalg)
  }
  else if (!"Gini" %in% ineq & "MLD" %in% ineq){
    tstatm <- pe_m/se_m
    pvalm <- 2*(pnorm(1 - abs(tstatm)))
    resm <- c(difference = pe_m, se = se_m, t.stat = tstatm,
              pvalue = pvalm)
  }
}
