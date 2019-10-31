clear;clc;

alpha = 0.1;
sigma = 0.2;
r = 0.05;
T = 0.5;
S0 = 100;

samples = 10;
steps = 1000;

%Flag = 0 for risk neutral; 1 for real world
%VRFlag = 0 for General MC Sim; 1 for Antithetic Variance Reduction
% [ matrix ] = GBM( samples, S0, alpha, sigma, r, T, steps, Flag, VRFlag )
data = GBM( samples, S0, alpha, sigma, r, T, steps, 0, 0);

plot(data);
