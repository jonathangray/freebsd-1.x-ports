      DOUBLE PRECISION FUNCTION VMNORM (N, V, W)
CLLL. OPTIMIZE
C-----------------------------------------------------------------------
C THIS FUNCTION ROUTINE COMPUTES THE WEIGHTED MAX-NORM
C OF THE VECTOR OF LENGTH N CONTAINED IN THE ARRAY V, WITH WEIGHTS
C CONTAINED IN THE ARRAY W OF LENGTH N..
C   VMNORM = MAX(I=1,...,N) ABS(V(I))*W(I)
C-----------------------------------------------------------------------
      INTEGER N,   I
      DOUBLE PRECISION V, W,   VM
      DIMENSION V(N), W(N)
      VM = 0.0D0
      DO 10 I = 1,N 
 10     VM = DMAX1(VM,DABS(V(I))*W(I))
      VMNORM = VM
      RETURN
C----------------------- END OF FUNCTION VMNORM ------------------------
      END 
