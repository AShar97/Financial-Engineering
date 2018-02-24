#Lookback Options - Efficient

# library("functools");
library("memoise");

rm(list = ls());

pos <- function(x){
  ind = which(x < 0)
  z = x
  z[ind] <- 0  ## z now contains the x^+
  return(z)
}

greater <- function(x, y){
  ind = which(x < y)
  z = x
  z[ind] <- y[ind]  ## z now contains the max(x,y) iterative.
  return(z)
}

v <- function( N, n, s, m ){
  if (N == n){
    return(m - s);
  }
  else{
    return( ( p_*v(N, n+1, u*s, greater(m, u*s))
              +
                q_*v(N, n+1, d*s, greater(m, d*s))
              )
            /
              R
            );
  }
}

memo_v <- memoise(v);
# memo_v <- Memoise(v);

markov_binopt <- function( S0, r, t, M, vol ){
  dt = t/M;
  
  time <- seq(0, t, by=dt);
  
  #Continuous Compounding so "exp(r*dt)".
  R <<- exp(r*dt);
  
  u <<- exp(vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  d <<- exp(-vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  
  if ((d > R) | (R > u)){
    stop('ArbitargePossible as "d < exp(r*dt) < u" not true.');
  }
  
  p_ <<- (R - d)/(u-d);
  q_ <<- (u - R)/(u-d);
  
  result <- memo_v(M, 0, S0, S0);
  
  return(result);
}

S0 = 100;
t = 1;
#M = c(5, 10, 25, 50);
M = c(5, 10, 25);
r = 0.08;
vol = 0.2;

for (i in 1:length(M)){
  cat("For M = ", M[i],", ");
  cat("Initial option price =", markov_binopt( S0, r, t, M[i], vol ), ".\n");
}

rm(list = ls())

