beta0 <- c(1,1,1,1,1,0,0,0,0,0)
betahat <- c(0.8,0.9,0.85,0,1.1,0.7,0,0,0,0)

summa <- function(beta0, betahat){
  n0 <- length(beta0)
  nhat <- length(betahat)
  if (n0 != nhat){
    stop("the length of betas are different")
  }

  TP = TN = FN = FP = 0

  betapre <- rep(0,nhat)
  betapre[betahat!=0] <- 1

  for (i in 1:n0){
    if(betapre[i]==1 & beta0[i]==1){TP = TP + 1}
    if(betapre[i]==1 & beta0[i]==0){FP = FP + 1}
    if(betapre[i]==0 & beta0[i]==1){FN = FN + 1}
    if(betapre[i]==0 & beta0[i]==0){TN = TN + 1}
  }

  cfmatrix <- matrix(c(TP,FN,FP,TN),nrow = 2)
  rownames(cfmatrix) <- c("estimate sig","estimate non sig")
  colnames(cfmatrix) <- c("true sig","true non sig")
  FDR <- FP/(TN+FP)
  FNR <- FN/(TP+FN)
  return(list(matrix = cfmatrix, FDR = FDR, FNR = FNR))
}