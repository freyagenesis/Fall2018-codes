

function [Q,A] = household(A)
% HouseHold Reflector  [Q,R] = household(X);
    % n = number of rows
    % p = predictors
[n,p] = size(A);
Q = eye(n);
for k = 1:p
    x = A(k:n,k);
    e = eye(n);
    V = sign(x(1)) * norm(x,2) * e(k:n,k) +x;
    V = V/norm(V);
    A(k:n,k:p) = A(k:n,k:p) - 2*V*(V'*A(k:n,k:p));
    for j = 1:n
           Q(k:n,j) = Q(k:n,j)- V*(2*(V'*Q(k:n,j)));
    end
end
Q = Q';
end
