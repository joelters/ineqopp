#' Estimate debiased Inequality of Opportunity (IOp) partial effects (PEs)
#'
#' `peffect` is a post-estimation function of IOp. By taking an estimate
#' of IOp and a vector of circumstance names, it compares IOp with all
#' circumstances and without each of the circumstances in the circs input
#' vector. Only possible with the crossfitted debiased estimator. Also, it is
#' advised to use the same ML and tuning parameters as in the IOp estimation
#' with all circumstances unless there is a specific reason to do otherwise.
#'
#' @param Y is a vector containing the (continuous) outcome of interest
#' @param X is a dataframe containing all the circumstances
#' @param circs vector of circumstances for which we want to compute the
#' partial effects.
#' @param FVs fitted values taken from running the function IOp first
##' @param ML is a string specifying which machine learner to use (usually the
##' same as the one used to estimate full IOp)
#' @param OLSensemble is a string vector specifying which learners should be
#' used in OLS ensemble method
#' @param SL.library is a string vector specifying which learners should be
#' used in SuperLearner
#' @param iop an IOp estimate based on the Gini using the full set of circumstances
#' @param pe_rel logical indicating whether relative IOp PEs should be computed
#' @param parallel logical indicating whether we want to parallelize the computation
#' of the partial effect of each circumstance
#' @param weights survey weights adding up to 1
#' #' @param rf.cf.ntree how many trees should be grown when using RF or CIF
#' @param rf.depth how deep should trees be grown in RF (NULL is full depth,
#' NULL in ranger)
#' @param cf.depth how deep should trees be grown in CIF (Inf is full depth,
#' as in partykit)
#' @param polynomial.Lasso degree of polynomial to be fitted when using Lasso.
#' 1 just fits the input X. 2 squares all variables and adds
#' all pairwise interactions. 3 squares and cubes all variables and adds all
#' pairwise and threewise interactions...
#' @param polynomial.Ridge degree of polynomial to be fitted when using Ridge,
#' see polynomial.Lasso for more info.
#' @param polynomial.loglin degree of polynomial to be fitted when using loglin,
#' see polynomial.Lasso for more info.
#' @param mtry number of variables to consider at each split in RF or CIF
#' @param xgb.nrounds s an integer specifying how many rounds to use in XGB
#' @param xgb.max.depth an integer specifying how deep trees should be grown in XGB
#' @param cb.iterations an integer specifying how many iterations to use in CB
#' @param cb.depth an integer specifying how deep trees should be grown in CB
#' @param torch.epochs an integer specifying the number of epochs (full passes through the dataset)
#'  to use when training the Torch neural network.
#' @param torch.hidden_units a numeric vector specifying the number of neurons in
#'  each hidden layer of the Torch neural network.
#' @param torch.lr a numeric value specifying the learning rate to be used for the
#' optimizer when training the Torch neural network.
#' @param torch.dropout a numeric value between 0 and 1 specifying the dropout
#' rate for regularization in the Torch neural network.
#' @param polynomial.NLLS_ext degree of polynomial to be fitted when using
#'  NLLS_exp, see polynomial.Lasso for more info.
#' @param start_nlls List with the starting values of the parameters.
#'  Default is log(mean(Y)) for the intercept and zero for all the rest.
#' @param ensemblefolds how many folds to use in crossvalidation for ensemble
#' methods (i.e. superlearner or OLSensemble)
#' @returns list containing PEs and relative PEs (if desired) estimates and standard
#' errors for each circumstance in circs
#' @examples
#'
#' n <- 3000
#' X1 <- rnorm(n)
#' X2 <- rnorm(n)
#' Y <- exp(2 + 0.2*X1 + 0.1*X2 + rnorm(n,0,0.5))
#' X <- data.frame(X1,X2)
#'
#' res <- IOp(Y,X,ML = "XGB", est_method = "Debiased",
#' fitted_values = TRUE, CFit = TRUE)
#'
#' FVs <- res$FVs
#' iop_pe <- res$IOp[1]
#'
#' pe2 <- peffect(Y,
#' X,
#' circs = c("X1", "X2"),
#' FVs = FVs,
#' ML = "XGB",
#' iop_full = iop_pe,
#' pe_rel = TRUE,
#' parallel = TRUE)
#' pe2$X1$PE_rel
#' pe2$X2$PE_rel
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#'
#' Terschuur, J. (2022). Debiased Machine Learning Inequality
#' of Opportunity in Europe. arXiv preprint arXiv:2212.02407.
#' @export
peffect <- function(Y,
                    X,
                    circs,
                    FVs,
                    ML = c("Lasso","Ridge","RF","CIF","XGB","CB","Torch",
                           "loglin", "NLLS_exp", "OLSensemble", "SL"),
                    OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                    SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                    iop_full = NULL,
                    pe_rel = FALSE,
                    parallel = FALSE,
                    group = FALSE,
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
                    start_nlls = NULL,
                    ensemblefolds = 5){

  peffect_aux <- function(Y,
                          X,
                          FVs,
                          ML = c("Lasso","Ridge","RF","CIF","XGB","CB","Torch",
                                 "loglin", "NLLS_exp", "OLSensemble", "SL"),
                          OLSensemble = c("Lasso","Ridge","RF","CIF","XGB","CB"),
                          SL.library = c("SL.ranger", "SL.xgboost","SL.glmnet"),
                          circ,
                          iop_full,
                          pe_rel = FALSE,
                          weights = NULL,
                          ensemblefolds = 5,
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
    if (ML == "Lasso" | ML == "Ridge"){
      Xk <- dplyr::select(as_tibble(X),-dplyr::starts_with(circ))
    }
    else{
      Xk <- dplyr::select(X,-all_of(circ))
    }

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
    if (pe_rel == TRUE){
      perel_g <- pe_g/iop_full
    } else{
      perel_g = NULL
    }


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

      if (pe_rel == TRUE){
        V_rel <- (1/(mean(Y)^2))*(((iopk_g^2)/(iop_full^4))*S11 +
                                    (1/(iop_full^2))*S22 -
                                    2*(iopk_g/(iop_full^3))*S12)
        se_rel_g <- sqrt(V_rel/n)
      } else{
        se_rel_g <- NULL
      }
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

      if (pe_rel == TRUE){
        V_rel <- (1/(weighted.mean2(Y,weights)^2))*(((iopk_g^2)/(iop_full^4))*S11 +
                                    (1/(iop_full^2))*S22 -
                                    2*(iopk_g/(iop_full^3))*S12)
        se_rel_g <- sqrt(V_rel)
      } else{
        se_rel_g <- NULL
      }
    }
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

  if (group == FALSE){
    if (parallel == TRUE){
      if (pe_rel == TRUE){
        n.cores <- min(parallel::detectCores(),length(circs))
        clust <- parallel::makeCluster(n.cores)
        parallel::clusterEvalQ(clust, set.seed(123))
        parallel::clusterExport(clust, varlist = ls(envir = environment()), envir = environment())
        pes <- parallel::parLapply(clust, circs,
                         function(x) peffect_aux(Y = Y,
                                                 X = X,
                                                 FVs = FVs,
                                                 ML = ML,
                                                 iop_full = iop_full,
                                                 OLSensemble = OLSensemble,
                                                 SL.library = SL.library,
                                                 circ = x,
                                                 pe_rel = TRUE,
                                                 weights = weights,
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
                                                 torch.dropout = torch.dropout))

        parallel::stopCluster(clust)
      } else{
        source()
        n.cores <- min(parallel::detectCores(),length(circs))
        clust <- parallel::makeCluster(n.cores)
        parallel::clusterEvalQ(clust, set.seed(123))
        parallel::clusterExport(clust, c("X", "Y", "FVs", "ML", "OLSensemble",
                                         "iop_full", "circs", "weights", "peffect_aux",
                                         "IOD", "SP_new", "as_tibble","dfnotl","iodnumtr",
                                         "weighted.mean2", "iodnumsq", "sumUw","se_deb",
                                         "SL.library", "rf.cf.ntree", "rf.depth",
                                         "cf.depth", "polynomial.Lasso",
                                         "polynomial.Ridge",
                                         "polynomial.loglin",
                                         "xgb.nrounds",
                                         "xgb.max.depth",
                                         "cb.iterations",
                                         "cb.depth",
                                         "torch.epochs",
                                         "torch.hidden_units",
                                         "torch.lr",
                                         "torch.dropout",
                                         "mtry",
                                         "polynomial.NLLS_exp",
                                         "start_nlls",
                                         "ensemblefolds",
                                         "peffect_aux"),
                                envir=environment())
        pes <- parLapply(clust, circs,
                         function(x) peffect_aux(Y = Y,
                                                 X = X,
                                                 FVs = FVs,
                                                 ML = ML,
                                                 OLSensemble = OLSensemble,
                                                 SL.library = SL.library,
                                                 iop_full = iop_full,
                                                 circ = x,
                                                 pe_rel = FALSE,
                                                 weights = weights,
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
                                                 torch.dropout = torch.dropout))

        stopCluster(clust)
      }
      names(pes) <- circs
      return(pes)
    }
    else if (parallel == FALSE){
      if (pe_rel == TRUE){
        pes <- lapply(circs, function(x) peffect_aux(Y = Y,
                                                     X = X,
                                                     FVs = FVs,
                                                     ML = ML,
                                                     OLSensemble = OLSensemble,
                                                     SL.library = SL.library,
                                                     iop_full = iop_full,
                                                     circ = x,
                                                     pe_rel = TRUE,
                                                     weights = weights,
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
                                                     torch.dropout = torch.dropout))

      } else {
        pes <- lapply(circs, function(x) peffect_aux(Y = Y,
                                                     X = X,
                                                     FVs = FVs,
                                                     ML = ML,
                                                     OLSensemble = OLSensemble,
                                                     SL.library = SL.library,
                                                     iop_full = iop_full,
                                                     circ = x,
                                                     pe_rel = FALSE,
                                                     weights = weights,
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
                                                     torch.dropout = torch.dropout))
      }
    }
    names(pes) <- circs
    return(pes)
  }
  else{

  }
}
