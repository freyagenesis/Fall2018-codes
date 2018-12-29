
%1
x = double(pi/3);
func1 = @sin;
func2 = @cos;
h = logspace(-1, -16);
approximate_drv = ((func1(x+h) - func1(x))./h);
disp(approximate_drv)
err = (func2(x) - approximate_drv).^2;
fprintf('%.4f\n', err);
fprintf('%.4f\n',approximate_drv);

loglog(h, err);
xlabel('Step size');
ylabel('Error');

%(b)
%The best step size is 10^-1 for finite difference.

%(c)
%When transitioning from 10^-1 to 10^-8, the error seems to decrease. But 
% as we move from 10^-8 to 10^-16, the error increases. 
%This happens because as the precision increases the error decreases.
%But after a certain point the (h) difference is not significant enough to
%make any difference using finite precision.
