#' Tests for significance of a group of circumstances
#'
#' `IOpgrouptest` takes the output of IOp estimation and a selection of
#' circumstances for which it performs a joint test.
#'
#' @param Y is a vector containing the (continuous) outcome of interest
#' @param X is a dataframe containing all the circumstances
#' @param circs vector of circumstances for which we want to perform the group test
#' @param FVs fitted values taken from running the function IOp first
#' @param ML is a string specifying which machine learner to use, usually one uses
#' the same ML as the one used in the estimation of IOp
#' @param ensemble is a string vector specifying which learners
#' should be used in the SuperLearner
#' @param iop_full an IOp estimate based on the Gini using the full set of circumstances
#' @param weights survey weights adding up to 1
#' @returns test results
#' @examples
#' n <- 3000
#' X1 <- rnorm(n)
#' X2 <- rnorm(n)
#' Y1 <- exp(2 + 0.2*X1 + 0.1*X2 + rnorm(n,0,0.5))
#' Y2 <- exp(2 + 0*X1 + 0.1*X2 + rnorm(n,0,0.5))
#' X <- data.frame(X1,X2)
#' ML = "XGB"
#' set.seed(123)
#'
#' res <- IOp(Y,X,ML = ML,sterr = TRUE, IOp_rel = TRUE,
#'            est_method = "Debiased", fitted_values = TRUE, CFit = TRUE,
#'            rf.depth = 5)
#'
#' FVs <- res$FVs
#' iop_pe <- res$IOp[1]
#' res$IOp
#' res$IOp_rel
#'
#' IOpgrouptest(Y1,X, FVs, ML = "XGB", iop_full = iop_pe, circs = c("X1"))
#' IOpgrouptest(Y2,X, FVs, ML = "XGB", iop_full = iop_pe, circs = c("X1"))
#'
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#'
#' Terschuur, J. (2022). Debiased Machine Learning Inequality
#' of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
IOpgrouptest <- function(Y,
                         X,
                         FVs,
                         ML = c("Lasso","Ridge","RF","CIF","XGB","CB","Torch",
                                "loglin", "NLLS_exp", "OLSensemble", "SL"),
                         iop_full = NULL,
                         circs,
                         OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                         SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                         ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                         ensemblefolds = 5,
                         weights = NULL,
                         rf.cf.ntree = 500,
                         rf.depth = 5,
                         cf.depth = 5,
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
                         mtry = max(floor(ncol(X)/3), 1),
                         polynomial.NLLS_exp = 1,
                         start_nlls = NULL){
  Xk <- dplyr::select(X,-all_of(circs))
  res <- IOD(Y = Y,
             X = Xk,
             CFit = TRUE,
             sterr = FALSE,
             IOp_rel = FALSE,
             ML = ML,
             OLSensemble = OLSensemble,
             SL.library = SL.library,
             ensemblefolds = ensemblefolds,
             rf.cf.ntree = rf.cf.ntree,
             rf.depth = rf.depth,
             cf.depth = cf.depth,
             polynomial.Lasso = polynomial.Lasso,
             polynomial.Ridge = polynomial.Ridge,
             polynomial.loglin = polynomial.loglin,
             polynomial.NLLS_exp = polynomial.NLLS_exp,
             start_nlls = start_nlls,
             mtry = mtry,
             xgb.nrounds = xgb.nrounds,
             xgb.max.depth = xgb.max.depth,
             cb.iterations = cb.iterations,
             cb.depth = cb.depth,
             torch.epochs = torch.epochs,
             torch.hidden_units = torch.hidden_units,
             torch.lr = torch.lr,
             torch.dropout = torch.dropout,
             fitted_values = TRUE,
             weights = weights)

  iopk_g <- res$IOp["IOp","IOp"]
  FVsk <- res$FVs
  pe_g <- iop_full - iopk_g

  n <- length(Y)
  S11 <- 0
  S22 <- 0
  S12 <- 0
  S <- 0
  if (is.null(weights)){
    a <- lapply(1:n, function(u){
      S11aux <- sum((1/(n-1))*(iop_full*(Y[u] + Y[-u]) -
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
  } else{
    a <- lapply(1:n, function(u){
      S11aux <- sum((1/(n-1))*(iop_full*(Y[u] + Y[-u]) -
                                 ((FVs[u] - FVs[-u] > 0) - (FVs[u] - FVs[-u] < 0))*(Y[u] - Y[-u])))
      S22aux <- sum((1/(n-1))*(iopk_g*(Y[u] + Y[-u]) -
                                 ((FVsk[u] - FVsk[-u] > 0) - (FVsk[u] - FVsk[-u] < 0))*(Y[u] - Y[-u])))
      if (u!=n){
        u1 <- u + 1
        WW <- sum(weights[u]*weights[u1:n])
      }
      else{
        WW <- 0
      }
      data.frame(S11aux = S11aux, S22aux = S22aux, WW = WW)
    })
    a <- do.call(rbind,a)
    WW <- sum(a$WW)
    wt2 <- (weights*(rep(sum(weights),length(Y)) - weights))/(2*WW)
    S11 <- sum(wt2^2)*sum((1/n)*a$S11aux^2)
    S22 <- sum(wt2^2)*sum((1/n)*a$S22aux^2)
    S12 <- sum(wt2^2)*sum((1/n)*a$S11aux*a$S22aux)
    V <- (1/(weighted.mean2(Y,weights)^2))*(S11 + S22 - 2*S12)
    se_g <- sqrt(V)
  }

  tstatg <- pe_g/se_g
  pvalg <- 2*(1 - pnorm(abs(tstatg)))
  resg <- c(difference = pe_g, se = se_g, t.stat = tstatg,
            pvalue = pvalg)
  return(resg)
}
