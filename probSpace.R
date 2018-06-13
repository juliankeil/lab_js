probSpace <- function(len){ 
  # PROBSPACE Determine the interval of percentiles 
  #   The function used equation (3) in Ulrich, Miller and Schroter (2007)
  P <- numeric(len);
  for(i in 1:len){
    P[i] <- (i - .5) / len;
  }
  return(P)
}
