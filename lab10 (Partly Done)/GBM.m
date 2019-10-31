function [ matrix ] = GBM( samples, S0, alpha, sigma, r, T, steps, Flag, VRFlag )
%GBM Summary of this function goes here
%   Detailed explanation goes here

dt = T/steps;

%Flag = 1 for real world; 0 for risk neutral
if (Flag == 1)
    mu = alpha;
else
    mu = r;
end

c = (mu - ((sigma^2)/2)) * dt;
sqrtdt = sqrt(dt);

%VRFlag = 0 for General MC Sim; 1 for Antithetic Variance Reduction
if (VRFlag == 0)
    matrix = zeros(steps, samples);
    matrix(1,:) = S0;
    for i=2:steps
        matrix(i,:) = matrix(i-1,:) .* exp(c + (sigma .* normrnd(0, sqrtdt,[1,samples])));
    end
else
    hsamples = round(samples/2);
    matrix = zeros(steps, samples);
    matrix(1,:) = S0;
    for i=2:steps
        zdt = normrnd(0, sqrtdt, [1,hsamples]);
        matrix(i,1:2:samples) = matrix(i-1,1:2:samples) .* exp(c + (sigma .* zdt));
        matrix(i,2:2:samples) = matrix(i-1,2:2:samples) .* exp(c + (sigma .* -zdt));
    end
end

end

