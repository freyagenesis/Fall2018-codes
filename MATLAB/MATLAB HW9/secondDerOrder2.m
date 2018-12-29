%(c)
function secondDerApprox_val = secondDerOrder2(y, h)
    l_of_y = length(y);
    % defining the array to contain
    %the second order
    %approximation to the second derivative
 
    secondDerApprox_val = zeros(1, l_of_y);
    secondDerApprox_val(1) = (y(l_of_y -1) - 2*y(1) + y(2))/(h*h);
    for i = 2:l_of_y-1
        secondDerApprox_val(i) = (y(i+1) - 2*y(i) + y(i-1))/(h*h);
    end
    secondDerApprox_val(l_of_y) = (y(l_of_y -1) - 2*y(l_of_y) + y(2))/(h*h);
end