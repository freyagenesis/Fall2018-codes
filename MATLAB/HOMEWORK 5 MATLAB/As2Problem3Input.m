
%Input matrix
% [-4, 3, -7; 0, 4, 7;0, -7, 5]
A = input('Enter the square matrix to be factorized');
%[10^-20, 1; 1, 1]
[Lower, Upper] = LUDecomp(A);


disp('The matrix to be decomposed is')
disp(A);
disp('The Lower Triangular Matrix is')
disp(Lower);
disp('The Upper Triangular Matrix is')
disp(Upper);

