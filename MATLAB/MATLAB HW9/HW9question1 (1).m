clc
clear
close all

syms x;

% first function y value
y = (sin(x)).^2;
% first function a0 value
a0 = (1/pi)*int(y, x, [-pi, pi]);
%when n = 5
n = 5;

sum = 0;

for vals = 1:n
   an = (1/pi)*int(y*cos(vals*x), x, [-pi, pi]); 
   bn = (1/pi)*int(y*sin(vals*x), x, [-pi, pi]);
   sum = sum + (an*cos(vals*x) + bn*sin(vals*x));
end
an;
% final function for the first function
final_a = (1/2)*a0 + sum;
%Plotting the first function
figure
ezplot(x,y,[-pi,pi])
hold on
fplot(final_a,[-pi,pi])

%Forming the second function
y1 = x + pi;
y2 = x;

sum=0;

% second function bias values
a0_1 = (1/pi)*int(y1, x, [-pi, 0]);
a0_2 = (1/pi)*int(y2, x, [0, pi]);

for vals = 1:n
   an = (1/pi)*int(y1*cos(vals*x), x, [-pi, 0])+(1/pi)*int(y2*cos(vals*x), x, [0,pi]); 
   bn = (1/pi)*int(y1*sin(vals*x), x, [-pi,0])+(1/pi)*int(y2*sin(vals*x), x, [0, pi]);
   sum = sum + (an*cos(vals*x) + bn*sin(vals*x));
end

% final function for the second function
final_b = (1/2)*(a0_1+a0_2) + sum;
%Plotting the second function
figure
ezplot(x,y1,[-pi,0])
hold on
ezplot(x,y2,[0,pi])
hold on
fplot(final_b,[-pi,pi])

%Same steps done for n = 100
n = 100;
a0 = (1/pi)*int(y, x, [-pi, pi]);

sum = 0;

for vals = 1:n
   %coefficients 
   an = (1/pi)*int(y*cos(vals*x), x, [-pi, pi]); 
   bn = (1/pi)*int(y*sin(vals*x), x, [-pi, pi]);
   sum = sum + (an*cos(vals*x) + bn*sin(vals*x));
end
%First function
final_a = (1/2)*a0 + sum;
figure
ezplot(x,y,[-pi,pi])
hold on
fplot(final_a,[-pi,pi])

y1 = x + pi;
y2 = x;
sum=0;
a0_1 = (1/pi)*int(y1, x, [-pi, 0]);
a0_2 = (1/pi)*int(y2, x, [0, pi]);

for vals = 1:n
   %coefficients 
   an = (1/pi)*int(y1*cos(vals*x), x, [-pi, 0])+(1/pi)*int(y2*cos(vals*x), x, [0,pi]); 
   bn = (1/pi)*int(y1*sin(vals*x), x, [-pi,0])+(1/pi)*int(y2*sin(vals*x), x, [0, pi]);
   sum = sum + (an*cos(vals*x) + bn*sin(vals*x));
end
%Second function
final_b = (1/2)*(a0_1+a0_2) + sum;

%Plotting second function
figure
ezplot(x,y1,[-pi,0])
hold on
ezplot(x,y2,[0,pi])
hold on
fplot(final_b,[-pi,pi])

%The plots shows us the as the harmonic 
%value increases it almosts interprets 
%the original plot except for the errors 
%introduced due to Gibbs Phenomena
