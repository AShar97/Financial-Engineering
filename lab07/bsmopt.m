function [ OptionValue ] = bsmopt( S, t, T, K, r, vol, Flag )
%BSMOPT Summary of this function goes here
%   Detailed explanation goes here

d1 = (log(S./K) + (r + (vol./2))*(T-t))./(vol .* sqrt(T-t));
d2 = d1 - (vol .* sqrt(T-t));

%Flag = 1 for a call option, or Flag = 0 for a put option.
if (Flag == 1)
    OptionValue = S.*normcdf(d1, 0, 1) - K.*exp(-r.*(T-t)).*normcdf(d2, 0, 1);
else
    OptionValue = K.*exp(-r.*(T-t)).*normcdf(-d2, 0, 1) - S.*normcdf(-d1, 0, 1);
end

end

