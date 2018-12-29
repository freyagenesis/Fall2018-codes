function [Q,R] = modi_gs(X)
    % Modified Gram-Schmidt.  [Q,R] = modi_gs(X);
    % n = number of rows
    % p = predictors
    [n,p] = size(X);
    Q = zeros(n,p);
    R = zeros(p,p);
    for i = 1:p
        V(:,i) = X(:,i);
    end
    for i = 1:p
    R(i,i) = norm(V(:,i));
    Q(:,i) = V(:,i)/R(i,i);
    for j = i+1:p
        R(i,j) = Q(:,i)'*V(:,j);
        V(:,j) = V(:,j)-R(i,j)*Q(:,i);
    end
end
