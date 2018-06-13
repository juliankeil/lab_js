ties <- function(W){
  # Count number k of unique values
  # and store these values in U.
  U <- NULL; W <- sort(W); n = length(W); k = 1; U[1] <- W[1]
  for (i in 2:n) {
    if (W[i] != W[i-1]) {
      k <- k+1;
      U <- cbind(U, W[i])
    }
  }
  U <- U[1,]
  
  # Determine number of replications R
  # k is the length of the vector, after trimming off the ties
  R <- numeric(k) 
  for (i in 1:k){
    for (j in 1:n){
      if (U[i] == W[j]) R[i] <- R[i] + 1;
    }
  }
  
  # Determine the cumlative frequency
  C <- numeric(k) 
  C[1] <- R[1]
  for(i in 2:k){
    C[i] <- C[i-1] + R[i];
  }
  res <- list(U, R, C)
  names(res) <- c("U", "R", "C")
  return(as.data.frame(res))
}
