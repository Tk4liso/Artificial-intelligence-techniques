function hijo=pareja(n,rank,mut,cross)

for i=1:size(n,1)/2
aux=rand*sum(rank);
padre=0;
while aux>0
    padre=padre+1;
    aux=aux-rank(padre);
end
aux=rand*sum(rank);
madre=0;
while aux>0
    madre=madre+1;
    aux=aux-rank(madre);
end

d=rand;  % decision de mutacion: muta con probabilidad d
hijo1=n(padre,:);
hijo2=n(madre,:);

if d<mut
    mt=fix(rand*10); % posicion de la mutacion
    lugar=round(rand*size(hijo1,2)*2);
    if lugar==0
        lugar=1;
    end
    if lugar>size(hijo1,2)
        lugar=lugar-size(hijo1,2);
        hijo2(lugar)=mt;
    else
        hijo1(lugar)=mt;
    end
end
dd=rand;
if dd<cross % decision de cruce: Cruza con probabilidad dd
    cr=round(rand*size(hijo1,2)); % lugar del cruce

aux=hijo1;
    hijo1(1:cr)=hijo2(1:cr);
    hijo2(1:cr)=aux(1:cr);
end

hijo(i*2-1,:)=hijo1;
hijo(i*2,:)=hijo2;

end


