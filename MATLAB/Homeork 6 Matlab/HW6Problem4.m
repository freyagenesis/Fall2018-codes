clc
clear
close 

m = 50;
n = 12;

t = linspace(0,1,50); 

A = fliplr(vander(t)); % creates a vandermonde matrix whose columns are powers of t anf flip it
A = A(:,1:12);
b = cos(4*t);

% (a) using normal equations
a_a = A.'*A;
b_a = A.'*b.';
X_n = a_a\b_a;

% (b) modified Gram-Schmidt function

[Q_m, R] = modi_gs(A);
X_m = R\(Q_m.'*b.');

% (c) householder reflector function

[Q_h, R_h] = household(A);
X_h = R_h\(Q_h.'*b.');

% (d) MATLAB qr function

[Q_q, R_q]=qr(A);
X_q=R_q\(Q_q.'*b.');

% (e) MATLAB A\b 

X_mat=A\b.';

figure
plot(X_n)
hold on;
plot(X_m)
hold on;
plot(X_h)
hold on;
plot(X_q)
hold on;
plot(X_mat)
legend('Nomral Equations','Modified GS','Householder','Using QR','Matlab A\b')

%Inference
%The outputs of different models were about the same with very minor
%differences which can be observed from the plot.




