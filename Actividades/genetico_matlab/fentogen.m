function pob=fentogen(n,min,max,largo)

for y=1:size(n,1) % poblacion
    for x=1:size(n,2) %genes
        
        o=(n(y,x)-min(x))/(max(x)-min(x));
        
        if o==1
            o=.9999;
        end
        
        if o==0
            o=.0001;
        end
        
        for i=1:largo
            pob(y,i+(x-1)*largo)= fix(o*10^i - fix(o*10^(i-1))*10);
        end
    end
end


    

