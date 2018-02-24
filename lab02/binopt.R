source("pos.R");

binopt <- function( S0, K, r, t, M, vol, Flag, uFlag ){
  dt = t/M;
  time <- seq(0, t, by=dt);
  if (uFlag == 'a'){
    u = exp(vol*sqrt(dt));
    d = exp(-vol*sqrt(dt));
  } else if (uFlag == 'b'){
    u = exp(vol*sqrt(dt) + (r-((vol^2)/2))*dt);
    d = exp(-vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  }
  
  #Continuous Compounding so "exp(r*dt)".
  if ((d > exp(r*dt)) | (exp(r*dt) > u)){
    stop('ArbitargePossible as "d < exp(r*dt) < u" not true.');
  }
  
  AssetPrice <- matrix(0, nrow = (M+1), ncol = (M+1));
  OptionValue <- matrix(0, nrow = (M+1), ncol = (M+1));
  
  AssetPrice[1,1] = S0;
  for (i in 2:(M+1)){
    AssetPrice[1, i] <- AssetPrice[1, (i-1)]*u;
    AssetPrice[2:i, i] <- AssetPrice[1:(i-1), (i-1)]*d;
  }
  
  #Flag = 1 for a call option, or Flag = 0 for a put option.
  if (Flag == 1){
    
    OptionValue[, M+1] <- pos(AssetPrice[, M+1] - K);
  }
  else{
    OptionValue[, M+1] <- pos(K - AssetPrice[, M+1]);
  }
  
  #Continuous Compounding so "exp(r*dt)".
  p_ = (exp(r*dt) - d)/(u-d);
  q_ = (u - exp(r*dt))/(u-d);
  
  for (i in seq(M, 1, by=-1)){
    OptionValue[1:i, i] <- (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt);
  }
  
  result <- list("AssetPrice" = AssetPrice, "OptionValue" = OptionValue, "time" = time);
  
  return(result);
}