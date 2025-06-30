function n=ran(min,max,num)
for x=1:size(min,2)
for i=1:num
    n(i,x)=rand*(max(x)-min(x))+min(x);
end
end