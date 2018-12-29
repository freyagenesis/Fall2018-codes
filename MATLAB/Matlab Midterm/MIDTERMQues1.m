%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


x = [1.02 0.95 0.87 0.77 0.67 0.56 0.44 0.3 0.16 0.01]
y = [0.39 0.32 0.27 0.22 0.18 0.15 0.13 0.12 0.13 0.15]

%(a)
%Creating a Vandermonde matrix
X = [transpose(y.*y) transpose(x.*y) transpose(x) transpose(y) transpose(ones(size(x)))]

%(b)
%To find the coefficient values 
B = X\transpose(x.*x)
%a b c d e =  -2.6356 0.1436 0.5514 3.2229 -0.4329          

%(c)
scatter(x, y)
hold on 
%Setting the calculated coefficient values
ezplot('-2.6356*y^2+0.1436*x*y+0.5514*x+3.2229*y -0.4329-x^2')
hold off
xlabel('X values')
ylabel('Y values')
