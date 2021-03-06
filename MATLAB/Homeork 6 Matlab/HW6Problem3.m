
% 3.(a)
x = [0 1 2 3 4 5 6];
b = [-0.002 1.1 1.98 3.05 3.95 5.1 6.02];

%A is the input matrix
A = [1 1 1 1 1 1 1; x ];
% L.U.X = b
[L,U]=lu(A');
% U.X = Y
Y=L\b';
% Using Y to get the X solution value
X=U\Y;
% Knowing the predicted values to be plotted with the actual
pred_vals_linear=A'*X;

plot(x,b,'o') %original data  
hold on;
plot(x,pred_vals_linear) %predicted values

%3.(b)
%Quadratic polynomial
x_square=x.^2;
A = [1 1 1 1 1 1 1;x ;x_square];
% L.U.X = b
[L,U]=lu(A');
% U.X = Y
Y=L\b';
% Using Y to get the X solution value
X=U\Y;
% Knowing the predicted values to be plotted with the actual
pred_vals_quad=A'*X;
plot(x,b,'o')  
hold on;
plot(x,pred_vals_linear,'o')
hold on;
plot(x,pred_vals_quad,'o')
legend('Original Data', 'Linear function', 'Quadratic function')
hold off;

%3.(c)
%Inference
%Since the linear and quadratic function are almost superimposing and 
%covering all data points there's not much of an advantage we observe 
%from the quadratic function. Its as good as the linear function.
%Hence the linear function seems to cover data points effectively as 
%inferred from the graph.