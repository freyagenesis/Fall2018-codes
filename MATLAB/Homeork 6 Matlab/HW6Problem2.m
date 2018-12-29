
qr_func_ans = [];
gs_func_ans = [];
modi_gs_func_ans = [];
household_func_ans = [];
vals = 2:10;
for i = 2:10
    %display = [" Q R for matrix of size", i];
    %disp(display)
    
    % By using qr function
    Q = qr(hilb(i));
    I = eye(i);
    qr_func_ans = [qr_func_ans norm(Q.'*Q - I)];
    %classical GS
    [cQ, cR] = gs(hilb(i));
    gs_func_ans = [gs_func_ans norm(cQ.'*cQ - I)];
    %modified GS
    [mQ, mR] = modi_gs(hilb(i))
    modi_gs_func_ans = [modi_gs_func_ans norm(mQ.'*mQ - I)];
    %Household
    [hQ, hR] = household(hilb(i))
    household_func_ans = [household_func_ans norm(hQ.'*hQ - I)];
    
end
hold on
plot(vals, qr_func_ans);
hold on
plot(vals, gs_func_ans);
hold on
plot(vals, modi_gs_func_ans);
hold on
plot(vals, household_func_ans);
legend('QR function', 'GramSchmidt' ,'Modified GramSchmidt', 'Household reflector')
hold off

%Inference
%The plots for Modified GramSchmidt, Household reflector overlap whereas QR
%function and Gram Schmidt shows the error is maximum in Classical Gram
%Schmidt followed by QR function.