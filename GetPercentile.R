GetPercentile <- function( P, G, tmax ){
  # Determine minimum of |G(Tp[i]) - P[i]|
  np <- length(P);
  Tp <- numeric(np)
  for( i in 1:np) {
    cc <- 100;
    for(t in 1:tmax) {
      if ( abs(G[t] - P[i]) < cc ) {
        c <- t;
        cc <- abs(G[t] - P[i]);
      }
    }
    
    if( P[i] > G[c] ){
      Tp[i] <-  c + (P[i] - G[c]) / (G[c+1] - G[c]);
    } else {
      Tp[i] <- c + (P[i] - G[c]) / (G[c] - G[c-1]);
    }
  }
  return( Tp )
}
