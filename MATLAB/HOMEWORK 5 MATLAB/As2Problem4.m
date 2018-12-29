
% declare tot_toc 
time_vals = 0; 
index = 0;
matrix_size = 10:40:1000;
time_taken = [];
disp("Execution Starts")
for i= matrix_size
    tic
    index = i-9;
    A = rand(i,i)+i*eye(i);
    [Lower, Upper] = LUDecomp(A);
    time_vals = time_vals+toc ;
    time_taken = [time_taken time_vals];
end

plot(matrix_size,time_taken);
xlabel('Matrix Size');
ylabel('Time Taken');

%No. 
%For small n, the trend does not hold because of the thus calculated time
%complexity on LU Decompisition.
%(2/3)*n^3 - (2/3)*n
%For large values of n, the second term can be neglected but not for
%smaller values.
