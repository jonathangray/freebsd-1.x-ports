      SUBROUTINE NNSC
     *     (N, R, C, IL, JL, IJL, L, D, IU, JU, IJU, U, Z, B, TMP)
CLLL. OPTIMIZE
C*** SUBROUTINE NNSC
C*** NUMERICAL SOLUTION OF SPARSE NONSYMMETRIC SYSTEM OF LINEAR
C      EQUATIONS GIVEN LDU-FACTORIZATION (COMPRESSED POINTER STORAGE) 
C
C
C       INPUT VARIABLES..  N, R, C, IL, JL, IJL, L, D, IU, JU, IJU, U, B
C       OUTPUT VARIABLES.. Z
C
C       PARAMETERS USED INTERNALLY..
C FIA   - TMP   - TEMPORARY VECTOR WHICH GETS RESULT OF SOLVING  LY = B.
C       -           SIZE = N. 
C
C  INTERNAL VARIABLES..
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW OF 
C      U OR L  TO BE USED.
C
      INTEGER R(1), C(1), IL(1), JL(1), IJL(1), IU(1), JU(1), IJU(1)
      DOUBLE PRECISION  L(1), D(1), U(1), B(1), Z(1), TMP(1), TMPK,SUM
C
C  ******  SET TMP TO REORDERED B  *************************************
      DO 1 K=1,N
   1    TMP(K) = B(R(K))
C  ******  SOLVE  LY = B  BY FORWARD SUBSTITUTION  *********************
      DO 3 K=1,N
        JMIN = IL(K)
        JMAX = IL(K+1) - 1
        TMPK = -D(K) * TMP(K) 
        TMP(K) = -TMPK
        IF (JMIN .GT. JMAX) GO TO 3
        ML = IJL(K) - JMIN
        DO 2 J=JMIN,JMAX
   2      TMP(JL(ML+J)) = TMP(JL(ML+J)) + TMPK * L(J)
   3    CONTINUE
C  ******  SOLVE  UX = Y  BY BACK SUBSTITUTION  ************************
      K = N
      DO 6 I=1,N
        SUM = -TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX) GO TO 5
        MU = IJU(K) - JMIN
        DO 4 J=JMIN,JMAX
   4      SUM = SUM + U(J) * TMP(JU(MU+J))
   5    TMP(K) = -SUM
        Z(C(K)) = -SUM
        K = K - 1
   6    CONTINUE
      RETURN
      END 
