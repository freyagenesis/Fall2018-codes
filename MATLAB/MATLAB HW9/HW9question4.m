% (b)
x=linspace(0, 50, 10);
xd = x*(180\pi); %theta values

plot(xd, xd)
hold on
y = sin(xd); %sin(theta) values
plot(xd, y)
legend("Theta", "Sin(Theta)")

%At the 8th index we find a difference of 0.05
diff_vals = (xd-y)./y

for i = 1:length(diff_vals)
    if diff_vals(i) < 0.0600;
        diff_vals(i) = 0;
        idx = i;
    end
end

diff_vals;

asin(y(idx))*180/pi

%For a theta value of 35.57 the small angle 
%approximation ceases to be valid, say
%5% error

%(f)

ts = [0 25];
%Initial Value condition
y_of_0 = [1.0 1.0];

[t,y] = ode45('func', ts, y_of_0 );
figure;
subplot(1,2,1)
plot(t,y(:,1),'-');
xlabel('time');

title('\theta (t)');

subplot(1,2,2)
plot(t,y(:,2),'-');
xlabel('time');

title('d \theta / dt (t)');
