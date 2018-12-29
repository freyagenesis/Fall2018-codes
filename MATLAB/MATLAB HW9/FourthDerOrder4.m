%(d)
function FourthDerApprox_val = FourthDerOrder4(B, h)
    
    FourthDerApprox_val = secondDerOrder2(B, h);
    FourthDerApprox_val = FourthDerApprox_val*((h*h)/12);
    FourthDerApprox_val = (B - FourthDerApprox_val);
  
end