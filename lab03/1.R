#American Options

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

binopt <- function( S0, K, r, t, M, vol, Flag ){
  dt = t/M;
  
  time <- seq(0, t, by=dt);
  
  u = exp(vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  d = exp(-vol*sqrt(dt) + (r-((vol^2)/2))*dt);
  
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
  else if (Flag == 0){
    OptionValue[, M+1] <- pos(K - AssetPrice[, M+1]);
  }
  
  #Continuous Compounding so "exp(r*dt)".
  p_ = (exp(r*dt) - d)/(u-d);
  q_ = (u - exp(r*dt))/(u-d);
  
  for (i in seq(M, 1, by=-1)){
    # for European Options:
    # OptionValue[1:i, i] <- (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt);
    # for American Options:
    if (Flag == 1){
      OptionValue[1:i, i] <- greater(pos(AssetPrice[1:i, i] - K), (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt));
    }
    else if (Flag == 0){
      OptionValue[1:i, i] <- greater(pos(K - AssetPrice[1:i, i]), (p_*OptionValue[1:i, i+1] + q_*OptionValue[2:(i+1), i+1])/exp(r*dt));
    }
  }
  
  result <- list("AssetPrice" = AssetPrice, "OptionValue" = OptionValue, "time" = time);
  
  return(result);
}

S0 = 100;
K = 100;
t = 1;
M = 100;
r = 0.08;
vol = 0.2;

cat("Initial call option price =", (binopt( S0, K, r, t, M, vol, 1 )$OptionValue)[1,1], ".\n");
cat("Initial put option price =", (binopt( S0, K, r, t, M, vol, 0 )$OptionValue)[1,1], ".\n");


##Part a.
S0 = 91:110;
ac <- 1:length(S0); ap <- 1:length(S0);

for (i in 1:length(S0)) {
  ac[i] <- (binopt( S0[i], K, r, t, M, vol, 1 )$OptionValue)[1,1];
  ap[i] <- (binopt( S0[i], K, r, t, M, vol, 0 )$OptionValue)[1,1];
}

pdf("1a.pdf");
par(mfrow=c(2,1));
plot(S0,ac, main="Call option", sub="S(0) vs. Initial Price",
     xlab="S0", ylab="Initial Price");
plot(S0,ap, main="Put option", sub="S(0) vs. Initial Price",
     xlab="S0", ylab="Initial Price");

dev.off();

S0 = 100;
#*#

##Part b.
K = 91:110;
bc <- 1:length(K); bp <- 1:length(K);

for (i in 1:length(K)) {
  bc[i] <- (binopt( S0, K[i], r, t, M, vol, 1 )$OptionValue)[1,1];
  bp[i] <- (binopt( S0, K[i], r, t, M, vol, 0 )$OptionValue)[1,1];
}

pdf("1b.pdf");
par(mfrow=c(2,1));
plot(K,bc, main="Call option", sub="K vs. Initial Price",
     xlab="K", ylab="Initial Price");
plot(K,bp, main="Put option", sub="K vs. Initial Price",
     xlab="K", ylab="Initial Price");

dev.off();

K = 100;
#*#

##Part c.
r = seq(0.05, 0.15, by=0.01);
cc <- 1:length(r); cp <- 1:length(r);

for (i in 1:length(r)) {
  cc[i] <- (binopt( S0, K, r[i], t, M, vol, 1 )$OptionValue)[1,1];
  cp[i] <- (binopt( S0, K, r[i], t, M, vol, 0 )$OptionValue)[1,1];
}

pdf("1c.pdf");
par(mfrow=c(2,1));
plot(r,cc, main="Call option", sub="r vs. Initial Price",
     xlab="r", ylab="Initial Price");
plot(r,cp, main="Put option", sub="r vs. Initial Price",
     xlab="r", ylab="Initial Price");

dev.off();

r = 0.08;
#*#

##Part d.
vol = seq(0.05, 0.35, by=0.01);
dc <- 1:length(vol); dp <- 1:length(vol);

for (i in 1:length(vol)) {
  dc[i] <- (binopt( S0, K, r, t, M, vol[i], 1 )$OptionValue)[1,1];
  dp[i] <- (binopt( S0, K, r, t, M, vol[i], 0 )$OptionValue)[1,1];
}

pdf("1d.pdf");
par(mfrow=c(2,1));
plot(vol,dc, main="Call option", sub="vol vs. Initial Price",
     xlab="vol", ylab="Initial Price");
plot(vol,dp, main="Put option", sub="vol vs. Initial Price",
     xlab="vol", ylab="Initial Price");

dev.off();

vol = 0.2;
#*#

##Part e.
M = seq(10, 200, by=5);
ec_k95 <- 1:length(M); ec_k100 <- 1:length(M); ec_k105 <- 1:length(M);
ep_k95 <- 1:length(M); ep_k100 <- 1:length(M); ep_k105 <- 1:length(M);

for (i in 1:length(M)) {
  ec_k95[i] <- (binopt( S0, 95, r, t, M[i], vol, 1 )$OptionValue)[1,1];
  ep_k95[i] <- (binopt( S0, 95, r, t, M[i], vol, 0 )$OptionValue)[1,1];
  
  ec_k100[i] <- (binopt( S0, 100, r, t, M[i], vol, 1 )$OptionValue)[1,1];
  ep_k100[i] <- (binopt( S0, 100, r, t, M[i], vol, 0 )$OptionValue)[1,1];
  
  ec_k105[i] <- (binopt( S0, 105, r, t, M[i], vol, 1 )$OptionValue)[1,1];
  ep_k105[i] <- (binopt( S0, 105, r, t, M[i], vol, 0 )$OptionValue)[1,1];
}

pdf("1e_k95.pdf");
par(mfrow=c(2,1));
plot(M,ec_k95, main="Call option with K=95", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,ep_k95, main="Put option with K=95", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");

dev.off();

pdf("1e_k100.pdf");
par(mfrow=c(2,1));
plot(M,ec_k100, main="Call option with K=100", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,ep_k100, main="Put option with K=100", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");

dev.off();

pdf("1e_k105.pdf");
par(mfrow=c(2,1));
plot(M,ec_k105, main="Call option with K=105", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,ep_k105, main="Put option with K=105", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");

dev.off();

M = 100;
#*#

rm(list = ls())

