clc
clear
close all

%time interval
t=[0 5];
%Initial condition
x_of_0=4;

diff_eq = @(t,x) -700*x-1000*exp(t);
%Using ODE45
[t_ode45,y] = ode45(diff_eq, t, x_of_0);
plot(t_ode45,y,'-o')
length(t_ode45)
hold on
%Using ODE23s
[t_ode23s,y] = ode23s(diff_eq, t, x_of_0);
plot(t_ode23s,y,'-o')
length(t_ode23s)

% 4237 steps were required by ode45
% 140 steps were required by ode23s