
% Input Matrix
X = [1,2,3;4,5,6;7,8,7;4,2,3;4,2,2];
% Gram-Schmidt
[Q,R] = gs(X)
% Modified Gram-Schmidt
[mQ,mR] = modi_gs(X)
% Household Reflector
[hQ, hR] = household(X)

%Inference
%On comparing the results thus obtained, and multiplying to get back the 
%input matrix, we see that the classical Gram Schmidt shows the maximum
%error followed by Household Reflector and then lastly the modified Gram 
%Schmidt.
