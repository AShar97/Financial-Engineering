#Lookback Options

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

binopt <- function( S0, r, t, M, vol ){
  dt = t/M;
  
  time <- seq(0, t, by=dt);
  
  u = exp(vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  d = exp(-vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  
  #Continuous Compounding so "exp(r*dt)".
  if ((d > exp(r*dt)) | (exp(r*dt) > u)){
    stop('ArbitargePossible as "d < exp(r*dt) < u" not true.');
  }
  
  AssetPrice <- matrix(0, nrow = (2^M), ncol = (M+1));
  OptionValue <- matrix(0, nrow = (2^M), ncol = (M+1));

  MaxAsset <- matrix(0, nrow = (2^M), ncol = (M+1));
  
  AssetPrice[1,1] = S0; MaxAsset[1,1] = S0;

  for (i in 2:(M+1)){
    AssetPrice[seq(1, 2^(i-1), 2), i] <- AssetPrice[(1:2^(i-2)), (i-1)]*u;
    AssetPrice[seq(2, 2^(i-1), 2), i] <- AssetPrice[(1:2^(i-2)), (i-1)]*d;

    MaxAsset[seq(1, 2^(i-1), 2), i] <- greater(AssetPrice[seq(1, 2^(i-1), 2), i], MaxAsset[(1:2^(i-2)), i-1]);
    MaxAsset[seq(2, 2^(i-1), 2), i] <- greater(AssetPrice[seq(2, 2^(i-1), 2), i], MaxAsset[(1:2^(i-2)), i-1]);
  }
  
  OptionValue[, M+1] <- (MaxAsset[, M+1] - AssetPrice[, M+1]);
  
  #Continuous Compounding so "exp(r*dt)".
  p_ = (exp(r*dt) - d)/(u-d);
  q_ = (u - exp(r*dt))/(u-d);
  
  for (i in seq(M, 1, by=-1)){
    #for European Options:
    #OptionValue[1:i, i] <- (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt);
    #for American Options:
    #if (Flag == 1){
    #  OptionValue[1:i, i] <- greater(pos(AssetPrice[1:i, i] - K), (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt));
    #}
    #else if (Flag == 0){
    #  OptionValue[1:i, i] <- greater(pos(K - AssetPrice[1:i, i]), (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt));
    #}
    #for Lookback Options:
    OptionValue[1:2^(i-1), i] <- (p_*OptionValue[seq(1, 2^i, 2), i+1] + q_*OptionValue[seq(2, 2^i, 2), i+1])/exp(r*dt);
  }
  
  result <- list("AssetPrice" = AssetPrice, "OptionValue" = OptionValue, "time" = time);
  
  return(result);
}

S0 = 100;
t = 1;
M = c(5, 10, 25, 50);
r = 0.08;
vol = 0.2;

# Time <- as.character(binopt( S0, r, t, 5, vol )$time);
OptionValue <- (binopt( S0, r, t, 5, vol )$OptionValue);


# write.csv(OptionValue, file = "2.csv", dec = ".", col.names = Time);

write.csv(OptionValue, file = "2.csv");

for (i in 1:length(M)){
  cat("For M = ", M[i],", ");
  cat("Initial option price =", (binopt( S0, r, t, M[i], vol )$OptionValue)[1,1], ".\n");
}

# OptionValue <- (binopt( S0, r, t, 5, vol )$OptionValue);

rm(list = ls())

