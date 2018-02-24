function [ AssetPrice, OptionValue, Time ] = binopt( S0, K, r, T, M, vol, Flag ) %Flag = 1 for Call; 0 for Put
%BINOPT Summary of this function goes here
%   Detailed explanation goes here
dt = T/M;

Time = (0:dt:T);

u = exp(vol*sqrt(dt) + (r-((vol^2)/2))*dt);
d = exp(-vol*sqrt(dt) + (r-((vol^2)/2))*dt);

%Continuous Compounding so "exp(r*dt)".
if (~(d < exp(r*dt)) || ~(exp(r*dt) < u))
    msgID = 'MYFUN:ArbitargePossible';
    msg = '"d < exp(r*dt) < u" not true.';
    baseException = MException(msgID,msg);
    throw(baseException)
end

AssetPrice = zeros(M+1, M+1);
OptionValue = zeros(M+1, M+1);

AssetPrice(1,1) = S0;
for i=2:(M+1)
    AssetPrice(1, i) = AssetPrice(1, (i-1))*u;
    AssetPrice(2:i, i) = AssetPrice(1:(i-1), (i-1))*d;
end

%Flag = 1 for a call option, or Flag = 0 for a put option.
if (Flag == 1)
    OptionValue(:, M+1) = max((AssetPrice(:, M+1) - K), 0);
else
    OptionValue(:, M+1) = max((K - AssetPrice(:, M+1)), 0);
end

%Continuous Compounding so "exp(r*dt)".
p_ = (exp(r*dt) - d)/(u-d);
q_ = (u - exp(r*dt))/(u-d);

for i = M:-1:1
    OptionValue(1:i, i) = (p_*OptionValue(1:i, i+1) + q_*OptionValue(2:(i+1), i+1))/exp(r*dt);
end

end

