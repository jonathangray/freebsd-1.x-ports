      DOUBLE PRECISION FUNCTION FNORM (N, A, W)
CLLL. OPTIMIZE
C-----------------------------------------------------------------------
C THIS FUNCTION COMPUTES THE NORM OF A FULL N BY N MATRIX,
C STORED IN THE ARRAY A, THAT IS CONSISTENT WITH THE WEIGHTED MAX-NORM
C ON VECTORS, WITH WEIGHTS STORED IN THE ARRAY W..
C   FNORM = MAX(I=1,...,N) ( W(I) * SUM(J=1,...,N) ABS(A(I,J))/W(J) ) 
C-----------------------------------------------------------------------
      INTEGER N,   I, J
      DOUBLE PRECISION A,   W, AN, SUM
      DIMENSION A(N,N), W(N)
      AN = 0.0D0
      DO 20 I = 1,N 
        SUM = 0.0D0 
        DO 10 J = 1,N
 10       SUM = SUM + DABS(A(I,J))/W(J) 
        AN = DMAX1(AN,SUM*W(I))
 20     CONTINUE
      FNORM = AN
      RETURN
C----------------------- END OF FUNCTION FNORM -------------------------
      END 
