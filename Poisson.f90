INTEGER FUNCTION poisson(lambda, kmax) RESULT(poiss_nb)

        REAL(kind = 16) :: lambda
        REAL(kind = 16) :: rand_nb
        REAL(kind = 16) :: CumulPoiss
        INTEGER :: i, ii
        INTEGER :: kmax
        REAL(kind = 16), DIMENSION(kmax) :: vect, SumPart

        !> Compute probabilities for each int. value and CDF values
        DO i = 1,kmax
           vect(i)    = exp(-lambda)*lambda**(i-1)/factorial_fun(i-1);
           SumPart(i) = sum(vect(1:i));
        END DO

        !> CDF expected to sum to 1
        CumulPoisson = sum(vect)

        !> Generate poisson distrib. int. (see Matlab. code for convg.)
        rand_nb = random_number 
        DO ii = 1,size(SumPart)-1
          IF (rand_nb .lt. SumPart(1)) THEN
              poiss_nb  = 0
          ELSE IF ((SumPart(ii).le.rand_nb) .and. (rand_nb .lt. SumPart(ii+1))) THEN
              poiss_nb = ii
          END IF
        END DO

END FUNCTION poisson

   !---------------------------------------------------------------------------
   ! FUNCTION factorial_fun(n)
   !
   !
   ! DESCRIPTION
   !> Gives the factorial of an integer
   !--------------------------------------------------------------------------
INTEGER FUNCTION factorial_fun(n)
    INTEGER :: ii
    INTEGER :: n
    factorial_fun = 1
    IF (n .lt. 0) THEN
        RETURN
    ELSE IF (n == 0) THEN
        factorial_fun = 1
    ELSE
        DO ii=1,n
         factorial_fun = factorial_fun*ii
        END DO
   END IF
END FUNCTION factorial_fun


