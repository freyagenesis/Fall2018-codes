function[Lower, Upper] = LUDecomp(A)
% Upper and lower Triangular matrices described
[m,n]=size(A);
Upper=zeros(m);
Lower=zeros(m);
for j=1:m
    Lower(j,j)=1;
end
for j=1:m
    Upper(1,j)=A(1,j);
end
for i=2:m
    for j=1:m
        for k=1:i-1
            E1=0;
            if k==1
                E1=0;
            else
            for p=1:k-1
                E1=E1+Lower(i,p)*Upper(p,k);
            end
            end
            Lower(i,k)=(A(i,k)-E1)/Upper(k,k);
        end
         for k=i:m
             E2=0;
           for p=1:i-1
               E2=E2+Lower(i,p)*Upper(p,k);
           end
           Upper(i,k)=A(i,k)-E2;
         end
    end
 end
end
