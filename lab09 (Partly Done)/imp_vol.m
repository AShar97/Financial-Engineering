function [ obs_vol ] = imp_vol( price, S0, r, T, K, Flag ) %Flag = 1 for Call; 0 for Put
%IMP_VOL Summary of this function goes here
%   Detailed explanation goes here

% syms vol;
% d1 = (log(S0/K) + ((r + ((vol^2)/2)) * T))/(vol*sqrt(T));
% d2 = d1 - (vol*sqrt(T));
% 
% %Flag = 1 for Call; 0 for Put
% if (Flag == 1)
%     eqn = ((S0 * normcdf(d1)) - (K * exp(-r*T) * normcdf(d2)) == price);
% else
%     eqn = ((K * exp(-r*T) * normcdf(-d2) - (S0 * normcdf(-d1))) == price);
% end
% 
% obs_vol = solve(eqn, vol);

d1 = @(vol) (log(S0/K) + ((r + ((vol^2)/2)) * T))/(vol*sqrt(T));
d2 = @(vol) d1(vol) - (vol*sqrt(T));

%Flag = 1 for Call; 0 for Put
if (Flag == 1)
    zero = @(vol) (((S0 * normcdf(d1(vol))) - (K * exp(-r*T) * normcdf(d2(vol)))) - price);
else
    zero = @(vol) ((K * exp(-r*T) * normcdf(-d2(vol)) - (S0 * normcdf(-d1(vol)))) - price);
end

dzero = @(vol) (S0 * sqrt(T) * normpdf(d1(vol)));

iter = 0; vol = 0; obs_vol = 1;
while ((abs(obs_vol - vol) > 10^-3) && (iter < 10^4))
    vol = obs_vol;
    obs_vol = vol - zero(vol)/dzero(vol);
end

end

