function dy = func(t,y)
dy = zeros(2,1);
w = 1;
dy(1) = y(2);
dy(2) = -w*w*sin(y(1));