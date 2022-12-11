function E = Gamma(Emax, theta, kappa)

 % GAMMA( EMAX, THETA, KAPPA)
 %==================
 %
 % Function generating Gamma distributed random values
 % for given parameters (theta, kappa) through randomly 
 % generated numbers uniformly between [0,1]
 %
 %
 % INPUT
 % -----
 %   - Emax  : highest val. to be gen.
 %   - theta : scale parameter
 %   - kappa : shape parameter
 %
 % Written by S.Guinchard (12/08/2022)
 % -----------------------------------

    x = linspace(0,Emax,1000);
    CDF = gammainc(x/theta,kappa); 
    beta = rand;
    [val, Ind] = min(abs(beta-CDF));
    E = x(Ind);
end

