function out=fun(x,y)
out=x.*x+y.*y+sqrt(sin(x*5).^2+sin(y*5).^2)*3+.1;  % funcion con minimos locales
%out=x.*x+y.*y+.01; % funcion con 1 minimo
