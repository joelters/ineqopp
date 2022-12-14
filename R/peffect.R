#' Estimate debiased Inequality of Opportunity (IOp) partial effects (PEs)
#'
#' `peffect` is a post-estimation function of IOp. By taking an estimate
#' of IOp and a vector of circumstance names, it compares IOp with all
#' circumstances and without each of the circumstances in the circs input
#' vector.
#'
#' @param Y is a vector containing the (continuous) outcome of interest
#' @param X is a dataframe containing all the circumstances
#' @param circs vector of circumstances for which we want to compute the
#' partial effects.
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
#' @param pe_rel logical indicating whether relative IOp PEs should be computed
#' @param parallel logical indicating whether we want to parallelize the computation
#' of the partial effect of each circumstance
#' @param weights survey weights adding up to 1
#' @returns list containing PEs and relative PEs (if desired) estimates and standard
#' errors for each circumstance in circs
#' @examples
#' mad <- mad2019[1:250,]
#' X <- dplyr::select(mad,-Y)
#' Y <- mad$Y
#' circs <- c("educM","educF")
#' iop <- IOp(Y,
#'            X,
#'            est_method = "Debiased",
#'            CFit = TRUE,
#'            ineq = c("Gini","MLD"),
#'            plugin_method = c("ML"),
#'            ML = "Lasso",
#'            sterr = FALSE,
#'            boots = 2,
#'            IOp_rel = TRUE,
#'            fitted_values = TRUE)
#' FVs <- iop$FVs
#' iop_gini <- iop$IOp["IOp","Gini"]
#' iop_mld <- iop$IOp["IOp","MLD"]
#'
#' pe1 <- peffect(Y,
#'                X,
#'                circs,
#'                FVs = FVs,
#'                ineq = c("Gini","MLD"),
#'                ML = "Lasso",
#'                K = 5,
#'                iop_gini = iop_gini,
#'                iop_mld = iop_mld,
#'                pe_rel = TRUE,
#'                parallel = FALSE)
#'
#' pe2 <- peffect(Y,
#'                X,
#'                circs,
#'                FVs = FVs,
#'                ineq = c("Gini","MLD"),
#'                ML = "Lasso",
#'                K = 5,
#'                iop_gini = iop_gini,
#'                iop_mld = iop_mld,
#'                pe_rel = TRUE,
#'                parallel = TRUE)
#'
#' @references Escanciano, J. C., & Terschuur, J. R. (2022).
#' Debiased Semiparametric U-Statistics: Machine Learning Inference
#' on Inequality of Opportunity. arXiv preprint arXiv:2206.05235.
#' @export
peffect <- function(Y,
                    X,
                    circs,
                    FVs,
                    ineq = c("Gini", "MLD",c("Gini","MLD")),
                    ML = c("Lasso","Ridge","RF","CIF","XGB","CB","SL"),
                    ensemble = c("SL.Lasso","SL.Ridge","SL.RF","SL.CIF","SL.XGB","SL.CB"),
                    K = 5,
                    iop_gini = NULL,
                    iop_mld = NULL,
                    pe_rel = FALSE,
                    parallel = FALSE,
                    group = FALSE,
                    weights = NULL){
  if (group == FALSE){
    if (parallel == TRUE){
      if (pe_rel == TRUE){
        n.cores <- min(parallel::detectCores(),length(circs))
        clust <- parallel::makeCluster(n.cores)
        parallel::clusterEvalQ(clust, set.seed(123))
        parallel::clusterExport(clust, c("X", "Y", "FVs", "ineq", "ML", "K", "iop_gini",
                               "iop_mld", "circs", "weights", "peffect_aux", "IOD", "SP",
                               "as_tibble","dfnotl","iodnumtr","weighted.mean2",
                               "iodnumsq", "sumUw","se_deb"),
                               envir=environment())
        pes <- parallel::parLapply(clust, circs,
                         function(x) peffect_aux(Y = Y,
                                                 X = X,
                                                 FVs = FVs,
                                                 ineq = ineq,
                                                 ML = ML,
                                                 ensemble = ensemble,
                                                 K = K,
                                                 iop_gini = iop_gini,
                                                 iop_mld = iop_mld,
                                                 circ = x,
                                                 pe_rel = TRUE,
                                                 weights = weights))

        parallel::stopCluster(clust)
      } else{
        n.cores <- min(parallel::detectCores(),length(circs))
        clust <- parallel::makeCluster(n.cores)
        parallel::clusterEvalQ(clust, set.seed(123))
        parallel::clusterExport(clust, c("X", "Y", "FVs", "ineq", "ML", "K", "iop_gini",
                                         "iop_mld", "circs", "weights", "peffect_aux", "IOD", "SP",
                                         "as_tibble","dfnotl","iodnumtr","weighted.mean2",
                                         "iodnumsq", "sumUw","se_deb"),
                                envir=environment())
        pes <- parLapply(clust, circs,
                         function(x) peffect_aux(Y = Y,
                                                 X = X,
                                                 FVs = FVs,
                                                 ineq = ineq,
                                                 ML = ML,
                                                 ensemble = ensemble,
                                                 K = K,
                                                 iop_gini = iop_gini,
                                                 iop_mld = iop_mld,
                                                 circ = x,
                                                 pe_rel = FALSE,
                                                 weights = weights))

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
                                                     ineq = ineq,
                                                     ML = ML,
                                                     ensemble = ensemble,
                                                     K = K,
                                                     iop_gini = iop_gini,
                                                     iop_mld = iop_mld,
                                                     circ = x,
                                                     pe_rel = TRUE,
                                                     weights = weights))

      } else {
        pes <- lapply(circs, function(x) peffect_aux(Y = Y,
                                                     X = X,
                                                     FVs = FVs,
                                                     ineq = ineq,
                                                     ML = ML,
                                                     ensemble = ensemble,
                                                     K = K,
                                                     iop_gini = iop_gini,
                                                     iop_mld = iop_mld,
                                                     circ = x,
                                                     pe_rel = FALSE,
                                                     weights = weights))
      }
    }
    names(pes) <- circs
    return(pes)
  }
  else{

  }
}
