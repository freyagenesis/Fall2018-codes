% 5
% Transition Matrix of Party A, Party B, Party C and Nonvoting
T = [0.7 0.2 0.2 0.1 ; 0.1 0.6 0.1 0.1 ; 0.1 0.2 0.6 0.1 ; 0.1 0 0.1 0.7];
% Initial probability vector consisting of the percentages of votes for
% each party and the non voters
p = [0.3 ; 0.15 ; 0.45 ; 0.1];

% (a)
% Likely outcome for next election
p1_a = T*p

% Likely outcome for next to next election
p2_a = T^2*p

% (b)
% Percentages having likely outcomes after a century
p100_b = T^100*p

% The solution states that 36% of the voters will vote for Party A and
% 24% will vote for Party C