      DOUBLE PRECISION FUNCTION BNORM (N, A, NRA, ML, MU, W)
CLLL. OPTIMIZE
C-----------------------------------------------------------------------
C THIS FUNCTION COMPUTES THE NORM OF A BANDED N BY N MATRIX,
C STORED IN THE ARRAY A, THAT IS CONSISTENT WITH THE WEIGHTED MAX-NORM
C ON VECTORS, WITH WEIGHTS STORED IN THE ARRAY W. 
C ML AND MU ARE THE LOWER AND UPPER HALF-BANDWIDTHS OF THE MATRIX.
C NRA IS THE FIRST DIMENSION OF THE A ARRAY, NRA .GE. ML+MU+1.
C IN TERMS OF THE MATRIX ELEMENTS A(I,J), THE NORM IS GIVEN BY..
C   BNORM = MAX(I=1,...,N) ( W(I) * SUM(J=1,...,N) ABS(A(I,J))/W(J) ) 
C-----------------------------------------------------------------------
      INTEGER N, NRA, ML, MU
      INTEGER I, I1, JLO, JHI, J
      DOUBLE PRECISION A, W
      DOUBLE PRECISION AN, SUM
      DIMENSION A(NRA,N), W(N)
      AN = 0.0D0
      DO 20 I = 1,N 
        SUM = 0.0D0 
        I1 = I + MU + 1
        JLO = MAX0(I-ML,1)
        JHI = MIN0(I+MU,N)
        DO 10 J = JLO,JHI
 10       SUM = SUM + DABS(A(I1-J,J))/W(J)
        AN = DMAX1(AN,SUM*W(I))
 20     CONTINUE
      BNORM = AN
      RETURN
C----------------------- END OF FUNCTION BNORM -------------------------
      END 
