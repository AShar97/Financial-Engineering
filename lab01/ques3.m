clear;clc;

S0 = 100;
K = 105;
T = 5;
r = 0.05;
vol = 0.3;

M = 20;

t = [0, 0.50, 1, 1.50, 3, 4.5];

idx = (t/(T/M)) + 1;

[ ~, CallOptionValue, Time ] = binopt( S0, K, r, T, M, vol, 1 );

[ ~, PutOptionValue, Time ] = binopt( S0, K, r, T, M, vol, 0 );

for i=1:length(t)
    disp(['T =', num2str(t(i))]);
    disp("Call option value:");
    disp(CallOptionValue(1:idx(i), idx(i)));
    
    disp("Put option value:");
    disp(PutOptionValue(1:idx(i), idx(i)));
end

