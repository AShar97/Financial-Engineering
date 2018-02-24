#Lookback Options - MC

rm(list = ls());

set.seed(47);

sample <- 1000000;

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

v <- function(M, S0, u, d, R){
  rv <- runif(sample*M);
  {
    ind = which(rv < 0.5);
    rv[ind] <- u;
    rv[-ind] <- d;
  }
  m <- c(rep(1, sample), rv);
  
  m <- matrix(data = m, nrow = sample, ncol = (M+1));
  for (i in 3:(M+1)){
    m[,i] <- m[,(i-1)]*m[,i];
  }
  max <- apply(m, 1, max);
  {
    max <- S0*max;
    SM <- S0*m[,(M+1)];
  }
  return(mean(max-SM)/(R^M));
}

mc_binopt <- function( S0, r, t, M, vol ){
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
  
  result <- v(M, S0, u, d, R);
  
  return(result);
}

S0 = 100;
t = 1;
M = c(5, 10, 25, 50);
r = 0.08;
vol = 0.2;

for (i in 1:length(M)){
  cat("For M = ", M[i],", ");
  cat("Initial option price =", mc_binopt( S0, r, t, M[i], vol ), ".\n");
}

rm(list = ls())

