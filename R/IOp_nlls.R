#' @export
IOp_nlls <- function(Y,
                     X,
                     est_method = c("Plugin","Debiased"),
                     ineq = c("Gini", "MLD",c("Gini", "MLD")),
                     sterr = TRUE,
                     sterr_type = 1,
                     CFit = TRUE,
                     IOp_rel = FALSE,
                     fitted_values = FALSE,
                     weights = NULL,
                     polynomial.nlls = 1,
                     FVs0 = NULL,
                     extFVs = NULL){
  if (sum(Y<0) != 0){stop("There are negative values for Y.")}
  if (est_method == "Plugin"){
    if(is.null(extFVs)){
      m <- ML::MLest(X,
                     Y,
                     ML = "NLLS_exp",
                     FVs = TRUE,
                     weights = weights)
      FVs <- m$FVs
    }
    else {FVs = extFVs}
    iopi <- unlist(acid::weighted.gini(FVs, weights)[2])
    if (sterr == TRUE){
      nn = length(Y)
      lhat = sapply(1:nn, function(u){
        (1/(nn-1))*sum((FVs[u] >= FVs[-u]) - (FVs[u] < FVs[-u]))
      })
      score = X*FVs
      alpham = ML::MLest(score,lhat,ML = "OLS")
      alpha = alpham$FVs
      se = se_deb_nlls(Y, FVs,alpha, iopi, weights = weights)
      iopi = c(iopi, se)
      names(iopi) = c("IOp", "se")
    }
    else {se = NULL}
    if (IOp_rel == TRUE){
      G <- acid::weighted.gini(Y,weights)
      G <- as.numeric(G[2])
      iorel <- iopi/G
      res <- c(iopi, iorel)
      names(res) = c(names(iopi),"IOp_rel")
      return(res)
    }
    else {return(iopi)}
  }
  else if (est_method == "Debiased"){
    if (CFit == FALSE){
      if (is.null(extFVs)){
        m <- ML::MLest(X,
                       Y,
                       ML = "NLLS_exp",
                       FVs = TRUE,
                       weights = weights)
        FVs <- m$FVs
      }
      else{FVs = extFVs}
      nn = length(Y)
      lhat = sapply(1:nn, function(u){
        (1/(nn-1))*sum((FVs[u] >= FVs[-u]) - (FVs[u] < FVs[-u]))
      })
      score = X*FVs
      alpham = ML::MLest(score,lhat,ML = "OLS")
      alpha = alpham$FVs
      n1 = nn - 1
      aux = sapply(1:n1, function(t){
        t1 = t + 1
        abs(FVs[t] - FVs[t1:nn]) + (alpha[t] - alpha[t1:nn])*(Y[t] - Y[t1:nn] - FVs[t] + FVs[t1:nn])
      })
      aux = (2/(nn*(nn-1)))*sum(aux)
      iodeb = aux/(2*mean(Y))
      if (sterr == TRUE){
        se = se_deb_nlls(Y,FVs,aplha,iodeb,weights)
      }
      else {se = NULL}
      if (IOp_rel == TRUE){
        G <- acid::weighted.gini(Y,weights)
        G <- as.numeric(G[2])
        iorel <- iodeb/G
        se_rel <- se_rel(Y,FVs,alpha, iodeb, IY = G, weights = weights)
        return(c(IOp = iodeb, IOp_rel = iorel, se = se, se_rel = se_rel))
      }
      else {return(c(IOp = iodeb, se = se))}
    }
    else{return(NULL)}
  }
}
