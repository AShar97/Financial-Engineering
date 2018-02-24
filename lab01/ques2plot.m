function [  ] = ques2plot( M, Callopt, Putopt )
%QUES2PLOT Summary of this function goes here
%   Detailed explanation goes here

%F = figure('Color','white', 'pos',[10 10 900 600]);
F = figure('Color','white');
set(gcf, 'Units', 'Normalized', 'OuterPosition', [0, 0.04, 1, 0.96]);
p = uipanel('Parent',F,'BorderType','none');
p.Title = ['Plot : Option pricing vs. M    {markers for alternate positions}']; 
p.TitlePosition = 'centertop';
p.FontSize = 12;
p.FontWeight = 'bold';

subplot(2,4,1, 'Parent',p);
plot(1:M, Callopt);
hold on;
scatter(1:2:M, Callopt(1:2:M), 'o');
scatter(2:2:M, Callopt(2:2:M), 'o');
hold off;
title("Call option \{M varying in steps of 1\}");

subplot(2,4,2, 'Parent',p);
plot(1:5:M, Callopt(1:5:M));
hold on;
scatter(1:10:M, Callopt(1:10:M), 'o');
scatter(6:10:M, Callopt(6:10:M), 'o');
hold off;
title("Call option \{M varying in steps of 5\}");

subplot(2,4,3:4, 'Parent',p);
plot(floor(M*.02):M, Callopt(floor(M*.02):M));
hold on;
scatter(floor(M*.02):2:M, Callopt(floor(M*.02):2:M), '.');
scatter(floor(M*.02)+1:2:M, Callopt(floor(M*.02)+1:2:M), '.');
hold off;
title("Call option \{for large M\}");

subplot(2,4,5, 'Parent',p);
plot(1:M, Putopt);
hold on;
scatter(1:2:M, Putopt(1:2:M), 'o');
scatter(2:2:M, Putopt(2:2:M), 'o');
hold off;
title("Put option \{M varying in steps of 1\}");

subplot(2,4,6, 'Parent',p);
plot(1:5:M, Putopt(1:5:M));
hold on;
scatter(1:10:M, Putopt(1:10:M), 'o');
scatter(6:10:M, Putopt(6:10:M), 'o');
hold off;
title("Put option \{M varying in steps of 5\}");

subplot(2,4,7:8, 'Parent',p);
plot(floor(M*.02):M, Putopt(floor(M*.02):M));
hold on;
scatter(floor(M*.02):2:M, Putopt(floor(M*.02):2:M), '.');
scatter(floor(M*.02)+1:2:M, Putopt(floor(M*.02)+1:2:M), '.');
hold off;
title("Put option \{for large M\}");

saveas(F,'2.jpg', 'jpg');

end

