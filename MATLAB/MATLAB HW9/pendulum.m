function dydt = pendulum(t,u)
    dydt = [u(2); -sin(u(1))];
end