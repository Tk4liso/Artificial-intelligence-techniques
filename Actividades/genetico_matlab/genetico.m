%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Algoritmo genetico para minimizacion de funciones bidimensionales 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set(gca,'nextplot','replacechildren');

%%% Parametros del algoritmo
genes=2;      %cromosoma-->(gen x,gen y)

largo=8;      %longitud del gen (debe ser par)
Ngen=80;      %cantidad de generaciones  
Nind=100;      %cantidad de individuos
mut=.2;       %tasa de mutacion
cross=.8;     %tasa de cruzamiento
xmin=-2;ymin=-2;xmax=-1;ymax=-1;  %rango de la poblacion inicial

%%% Paisaje energetico a explorar
min=[-2,-2];  %rango de la funcion min=[min(x),min(y)], max=[max(x),max(y)]
max=[2,2];
x=min(1):(max(1)-min(1))/100:max(1);                    %  
y=min(2):(max(2)-min(2))/100:max(2);                    %
[X,Y] = meshgrid(x,y);                                  %            
z=fun(X,Y) ;  %fun es el paisaje de energia a estudiar  % 
figure(1)						%
surfc(X,Y,z)			       			%  Potencial
pause(5)     %Muestra la superficie durante 5 segundos

%%% Eleccion de la poblacion inicial de manera aleatoria en el rango elegido
% ran([xmin ymin],[xmax ymax],# individuos)
% ran toma tres argumentos y devuelve  n=(iind,1)-->coord x del individuo iind 
n=ran([xmin ymin],[xmax ymax],Nind);  %n=(iind,2)-->coord y del individuo iind

%%% se plotea la poblacion y las lineas de nivel de la funcion de fitness
figure(2)
plot(n(:,1),n(:,2),'ro');
hold on
contour(X,Y,fun(X,Y));
hold off
axis([min(1) max(1) min(2) max(2)])
HH=gcf;
F(:,1)=getframe(HH);

%%% Ciclo principal
% iter=:# generacion
for iter=1:Ngen                   
rank=1./fun(n(:,1),n(:,2));   %Funcion de fitness-->1/fun
fitness(iter)=sum(rank);

% fentogen toma el fenotipo (coordenadas reales x e y) y devuelve 
% el genotipo (los "largo=4" digitos significativos), por ejemplo transforma
% 0.7854 en (7),(8),(5),(4). 
n=fentogen(n,min,max,largo);

% pareja selecciona padre y madre de acuerdo a la "regla de la ruleta"
% Luego de una posible mutacion y un posible cruzamiento entre ambos 
% los dos nuevos hijos reemplazan a padre y madre.
n=pareja(n,rank,mut,cross);

% gentofen toma el genotipo de cada individuo y devuelve
% el fenotipo.
n=gentofen(n,min,max,largo);

% se plotea la poblacion y las lineas de nivel de la funcion de fitness
plot(n(:,1),n(:,2),'ro');
title( ['Generacion # ', int2str(iter)] );
hold on
contour(X,Y,fun(X,Y));
hold off
axis([min(1) max(1) min(2) max(2)])
F(:,iter+1)=getframe(HH);
end

%Reproduccion de la evolucion temporal
movie(F,2);
%Evolucion de la funcion de fitness
figure(3)
plotfitness=plot(fitness,'b:o');
set(plotfitness,'MarkerSize',10);
xlabel('generacion','FontSize',16)
ylabel('fitness','FontSize',16)


