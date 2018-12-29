% 4
%(a)
% Transition Matrix
M_a = [1/4 1/3 ; 3/4 2/3];
% Initial probability vector
p_a = [2/3 ; 1/3];
% Probability Vector p1
p1_a = M_a*p_a
% Probability Vector p2
p2_a = M_a^2*p_a

%(b)
% Transition Matrix
M_b = [1/2 1/3 0 ; 0 2/3 1/2 ; 1/2 0 1/2];
% Initial probability vector
p_b = [1/3 ; 1/6 ; 1/2];
% Probability Vector p1
p1_b = M_b*p_b
% Probability Vector p2
p2_b = M_b^2*p_b


%(c)
% Transition Matrix
M_c = [1/4 1/3 1/2 ; 1/2 1/3 1/6 ; 1/4 1/3 1/3];
% Initial probability vector
p_c = [1/4 ; 1/2 ; 1/4];
% Probability Vector p1
p1_c = M_c*p_c
% Probability Vector p2
p2_c = M_c^2*p_c