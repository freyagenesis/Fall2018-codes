clear;
clc;
close all;
%(e)
%Describing the linspace
x = linspace(0, 2*pi, 10);

y = sin(x);
%Grid size
h = (2*pi)/10;

B = secondDerOrder2(y,h);
%Using the result of secord derivative approximation 
%into fourth order
C = FourthDerOrder4(B,h);

y_dif = -sin(x);

figure
plot(x, B,'r');
hold on
plot(x, y_dif,'b');
legend("Second derivative function", "Original function")

figure
plot(x, C,'r');
hold on
plot(x, y_dif,'b');
legend("Fourth derivative function", "Original function")
hold off

%(f)
%Error from second order deriv
errors_sec = zeros(1, 100);
%Error from fourth order deriv
errors_four = zeros(1, 100);

i = 1;

for grid_size = 100:100:10000
        
    x = linspace(0, 2*pi, grid_size);
    y = sin(x);
    y_dif = -sin(x);
    h = (2*pi)/grid_size;
    B = secondDerOrder2(y,h);
    C = FourthDerOrder4(B,h);
    
    errors_sec(i) = norm(y_dif - B);
    errors_four(i) = norm(y_dif - C);
    i=i+1;
    
end

figure
loglog(100:100:10000, errors_sec)
hold on
loglog(100:100:10000, errors_four)
legend("Second derivative function", "Fourth derivative function")
hold off
%As h increases, error decreases. Compared to the second derivative,
%the fourth derivative is more accurate as it has lesser errors.

%The error decreases of the order 4 as we take the 
%fourth order derivative. In the fourth order scheme, the 
%error value decreases four fold O(h4) and the wave thus 
%obtained is more closer to the original wave.

%(g)
%Error from second order deriv
errors_sec = zeros(1, 100);
%Error from fourth order deriv
errors_four = zeros(1, 100);

i = 1;
j = 1;

for p = 2:2:8
    i = 1;
    for grid_size = 100:100:10000
    
        x = linspace(0, 2*pi, grid_size);
        h = (2*pi)/grid_size;
        y = sin(x);
        %function with random error
        err_y = y + 2*(h^p)*(rand(size(y))+0.5);
        y_dif = -sin(x);
    
        B = secondDerOrder2(err_y,h);
        C = FourthDerOrder4(B,h);
    
        errors_sec(i) = norm(y_dif - B);
        errors_four(i) = norm(y_dif - C);
        i = i+1;
    end
    subplot(2,2,j)
    j = j+1;
    loglog(100:100:10000, errors_sec)
    hold on
    loglog(100:100:10000, errors_four)
    legend("2nd deriv func", "4th deriv func")
    title(["P=", num2str(p)])
    hold on
end
