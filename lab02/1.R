#European Options

rm(list = ls());

pos <- function(x){
  ind = which(x < 0)
  z = x
  z[ind] <- 0  ## z now contains the x^+
  return(z)
}

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
  else if (Flag == 0){
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

S0 = 100;
K = 100;
t = 1;
M = 100;
r = 0.08;
vol = 0.2;

cat("Set 1.\n");
cat("Initial call option price =", (binopt( S0, K, r, t, M, vol, 1 , 'a' )$OptionValue)[1,1], ".\n");
cat("Initial put option price =", (binopt( S0, K, r, t, M, vol, 0 , 'a' )$OptionValue)[1,1], ".\n");

cat("Set 2.\n");
cat("Initial call option price =", (binopt( S0, K, r, t, M, vol, 1 , 'b' )$OptionValue)[1,1], ".\n");
cat("Initial put option price =", (binopt( S0, K, r, t, M, vol, 0 , 'b' )$OptionValue)[1,1], ".\n");

##Part a.
S0 = 91:110;
a1c <- 1:length(S0); a1p <- 1:length(S0);
a2c <- 1:length(S0); a2p <- 1:length(S0);

for (i in 1:length(S0)) {
  a1c[i] <- (binopt( S0[i], K, r, t, M, vol, 1 , 'a' )$OptionValue)[1,1];
  a1p[i] <- (binopt( S0[i], K, r, t, M, vol, 0 , 'a' )$OptionValue)[1,1];
  a2c[i] <- (binopt( S0[i], K, r, t, M, vol, 1 , 'b' )$OptionValue)[1,1];
  a2p[i] <- (binopt( S0[i], K, r, t, M, vol, 0 , 'b' )$OptionValue)[1,1];
}
pdf("1a.pdf");
par(mfrow=c(2,2));
plot(S0,a1c, main="Call option for Set1", sub="S(0) vs. Initial Price",
     xlab="S0", ylab="Initial Price");
plot(S0,a1p, main="Put option for Set1", sub="S(0) vs. Initial Price",
     xlab="S0", ylab="Initial Price");
plot(S0,a2c, main="Call option for Set2", sub="S(0) vs. Initial Price",
     xlab="S0", ylab="Initial Price");
plot(S0,a2p, main="Put option for Set2", sub="S(0) vs. Initial Price",
     xlab="S0", ylab="Initial Price");

dev.off();

S0 = 100;
#*#

##Part b.
K = 91:110;
b1c <- 1:length(K); b1p <- 1:length(K);
b2c <- 1:length(K); b2p <- 1:length(K);

for (i in 1:length(K)) {
  b1c[i] <- (binopt( S0, K[i], r, t, M, vol, 1 , 'a' )$OptionValue)[1,1];
  b1p[i] <- (binopt( S0, K[i], r, t, M, vol, 0 , 'a' )$OptionValue)[1,1];
  b2c[i] <- (binopt( S0, K[i], r, t, M, vol, 1 , 'b' )$OptionValue)[1,1];
  b2p[i] <- (binopt( S0, K[i], r, t, M, vol, 0 , 'b' )$OptionValue)[1,1];
}

pdf("1b.pdf");
par(mfrow=c(2,2));
plot(K,b1c, main="Call option for Set1", sub="K vs. Initial Price",
     xlab="K", ylab="Initial Price");
plot(K,b1p, main="Put option for Set1", sub="K vs. Initial Price",
     xlab="K", ylab="Initial Price");
plot(K,b2c, main="Call option for Set2", sub="K vs. Initial Price",
     xlab="K", ylab="Initial Price");
plot(K,b2p, main="Put option for Set2", sub="K vs. Initial Price",
     xlab="K", ylab="Initial Price");

dev.off();

K = 100;
#*#

##Part c.
r = seq(0.05, 0.15, by=0.01);
c1c <- 1:length(r); c1p <- 1:length(r);
c2c <- 1:length(r); c2p <- 1:length(r);

for (i in 1:length(r)) {
  c1c[i] <- (binopt( S0, K, r[i], t, M, vol, 1 , 'a' )$OptionValue)[1,1];
  c1p[i] <- (binopt( S0, K, r[i], t, M, vol, 0 , 'a' )$OptionValue)[1,1];
  c2c[i] <- (binopt( S0, K, r[i], t, M, vol, 1 , 'b' )$OptionValue)[1,1];
  c2p[i] <- (binopt( S0, K, r[i], t, M, vol, 0 , 'b' )$OptionValue)[1,1];
}

pdf("1c.pdf");
par(mfrow=c(2,2));
plot(r,c1c, main="Call option for Set1", sub="r vs. Initial Price",
     xlab="r", ylab="Initial Price");
plot(r,c1p, main="Put option for Set1", sub="r vs. Initial Price",
     xlab="r", ylab="Initial Price");
plot(r,c2c, main="Call option for Set2", sub="r vs. Initial Price",
     xlab="r", ylab="Initial Price");
plot(r,c2p, main="Put option for Set2", sub="r vs. Initial Price",
     xlab="r", ylab="Initial Price");

dev.off();

r = 0.08;
#*#

##Part d.
vol = seq(0.05, 0.35, by=0.01);
d1c <- 1:length(vol); d1p <- 1:length(vol);
d2c <- 1:length(vol); d2p <- 1:length(vol);

for (i in 1:length(vol)) {
  d1c[i] <- (binopt( S0, K, r, t, M, vol[i], 1 , 'a' )$OptionValue)[1,1];
  d1p[i] <- (binopt( S0, K, r, t, M, vol[i], 0 , 'a' )$OptionValue)[1,1];
  d2c[i] <- (binopt( S0, K, r, t, M, vol[i], 1 , 'b' )$OptionValue)[1,1];
  d2p[i] <- (binopt( S0, K, r, t, M, vol[i], 0 , 'b' )$OptionValue)[1,1];
}

pdf("1d.pdf");
par(mfrow=c(2,2));
plot(vol,d1c, main="Call option for Set1", sub="vol vs. Initial Price",
     xlab="vol", ylab="Initial Price");
plot(vol,d1p, main="Put option for Set1", sub="vol vs. Initial Price",
     xlab="vol", ylab="Initial Price");
plot(vol,d2c, main="Call option for Set2", sub="vol vs. Initial Price",
     xlab="vol", ylab="Initial Price");
plot(vol,d2p, main="Put option for Set2", sub="vol vs. Initial Price",
     xlab="vol", ylab="Initial Price");

dev.off();

vol = 0.2;
#*#

##Part e.
M = seq(10, 200, by=5);
e1c_k95 <- 1:length(M); e1c_k100 <- 1:length(M); e1c_k105 <- 1:length(M);
e1p_k95 <- 1:length(M); e1p_k100 <- 1:length(M); e1p_k105 <- 1:length(M);
e2c_k95 <- 1:length(M); e2c_k100 <- 1:length(M); e2c_k105 <- 1:length(M);
e2p_k95 <- 1:length(M); e2p_k100 <- 1:length(M); e2p_k105 <- 1:length(M);

for (i in 1:length(M)) {
  e1c_k95[i] <- (binopt( S0, 95, r, t, M[i], vol, 1 , 'a' )$OptionValue)[1,1];
  e1p_k95[i] <- (binopt( S0, 95, r, t, M[i], vol, 0 , 'a' )$OptionValue)[1,1];
  e2c_k95[i] <- (binopt( S0, 95, r, t, M[i], vol, 1 , 'b' )$OptionValue)[1,1];
  e2p_k95[i] <- (binopt( S0, 95, r, t, M[i], vol, 0 , 'b' )$OptionValue)[1,1];
  
  e1c_k100[i] <- (binopt( S0, 100, r, t, M[i], vol, 1 , 'a' )$OptionValue)[1,1];
  e1p_k100[i] <- (binopt( S0, 100, r, t, M[i], vol, 0 , 'a' )$OptionValue)[1,1];
  e2c_k100[i] <- (binopt( S0, 100, r, t, M[i], vol, 1 , 'b' )$OptionValue)[1,1];
  e2p_k100[i] <- (binopt( S0, 100, r, t, M[i], vol, 0 , 'b' )$OptionValue)[1,1];
  
  e1c_k105[i] <- (binopt( S0, 105, r, t, M[i], vol, 1 , 'a' )$OptionValue)[1,1];
  e1p_k105[i] <- (binopt( S0, 105, r, t, M[i], vol, 0 , 'a' )$OptionValue)[1,1];
  e2c_k105[i] <- (binopt( S0, 105, r, t, M[i], vol, 1 , 'b' )$OptionValue)[1,1];
  e2p_k105[i] <- (binopt( S0, 105, r, t, M[i], vol, 0 , 'b' )$OptionValue)[1,1];
}

pdf("1e_k95.pdf");
par(mfrow=c(2,2));
plot(M,e1c_k95, main="Call option for Set1 with K=95", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e1p_k95, main="Put option for Set1 with K=95", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e2c_k95, main="Call option for Set2 with K=95", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e2p_k95, main="Put option for Set2 with K=95", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");

dev.off();

pdf("1e_k100.pdf");
par(mfrow=c(2,2));
plot(M,e1c_k100, main="Call option for Set1 with K=100", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e1p_k100, main="Put option for Set1 with K=100", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e2c_k100, main="Call option for Set2 with K=100", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e2p_k100, main="Put option for Set2 with K=100", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");

dev.off();

pdf("1e_k105.pdf");
par(mfrow=c(2,2));
plot(M,e1c_k105, main="Call option for Set1 with K=105", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e1p_k105, main="Put option for Set1 with K=105", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e2c_k105, main="Call option for Set2 with K=105", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");
plot(M,e2p_k105, main="Put option for Set2 with K=105", sub="M vs. Initial Price",
     xlab="M", ylab="Initial Price");

dev.off();

M = 100;
#*#

rm(list = ls())

