cdf.ulrich <- function(data=NULL, maximum=3000){
  # Create a container, whose length is the longest data vector
  # data is the output from `ties` function
  U <- data[,1]
  R <- data[,2]
  C <- data[,3]
  G <- numeric(maximum);
  
  # The length of the processed data vector, trimming off ties, if there is any.
  k <- length(U);  # U contains data in millisecond, e.g., 320 ms etc. 
  
  # The last element of the cumulative frequency supposely is the 
  # length of the data vector.
  n <- C[k]
  
  for(i in 1:k) { U[i] <- round(U[i]) }
  
  # from 1 ms to the first value of the data set, their probability should be 0.
  for(t in 1:U[1]) { G[t] <- 0 }   
  
  for(t in U[1]:U[2]){
    G[t] <- ( R[1]/2 + (R[1]+R[2]) / 2*(t-U[1]) / (U[2] - U[1]) ) / n;
  }
  
  for(i in 2:(k-1)){
    for(t in U[i]:U[i+1]){
      G[t] <- (C[i-1] + R[i] / 2+(R[i] +R[i+1]) / 2*(t-U[i]) / (U[i+1] - U[i])) / n;
    }
  }
  
  for(t in U[k]:maximum){
    G[t] <- 1;
  }
  return(G)
}
