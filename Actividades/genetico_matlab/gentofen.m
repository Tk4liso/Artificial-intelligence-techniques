function pob=gentofen(n,min,max,largo)

pob=zeros(size(n,1),size(n,2)/largo);
for y=1:size(n,1) % poblacion
    for gen=1:size(n,2)/largo
        ex=0;
        for x=1+(gen-1)*largo:gen*largo %genes
        ex=ex+1;
       pob(y,gen)=pob(y,gen)+n(y,x)/10^ex;
    end
end
end
for x=1:size(n,2)/largo
    pob(:,x)=pob(:,x)*(max(x)-min(x))+min(x);
end


    

