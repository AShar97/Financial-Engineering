clear;clc;

S0 = 100;
K = 105;
T = 5;
r = 0.05;
vol = 0.3;

M = 5000;

Callopt = 1:M; Putopt = 1:M;

for i=1:M
    [ ~, OptionValue, ~ ] = binopt( S0, K, r, T, i, vol, 1 );
    Callopt(i) = OptionValue(1,1);
    [ ~, OptionValue, ~ ] = binopt( S0, K, r, T, i, vol, 0 );
    Putopt(i) = OptionValue(1,1);
end

ques2plot( M, Callopt, Putopt );

% save("ques2workspace");

