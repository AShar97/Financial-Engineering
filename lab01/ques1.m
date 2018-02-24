clear;clc;

S0 = 100;
K = 105;
T = 5;
r = 0.05;
vol = 0.3;

M=[1, 5, 10, 20, 50, 100, 200, 400];

Callopt = 1:length(M); Putopt = 1:length(M);

for i=1:length(M)
    [ ~, OptionValue, ~ ] = binopt( S0, K, r, T, M(i), vol, 1 );
    Callopt(i) = OptionValue(1,1);
    [ ~, OptionValue, ~ ] = binopt( S0, K, r, T, M(i), vol, 0 );
    Putopt(i) = OptionValue(1,1);
end

