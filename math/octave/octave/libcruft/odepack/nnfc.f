      SUBROUTINE NNFC
     *     (N, R,C,IC, IA,JA,A, Z, B,
     *      LMAX,IL,JL,IJL,L, D, UMAX,IU,JU,IJU,U,
     *      ROW, TMP, IRL,JRL, FLAG)
CLLL. OPTIMIZE
C*** SUBROUTINE NNFC
C*** NUMERICAL LDU-FACTORIZATION OF SPARSE NONSYMMETRIC MATRIX AND
C      SOLUTION OF SYSTEM OF LINEAR EQUATIONS (COMPRESSED POINTER
C      STORAGE)
C
C
C       INPUT VARIABLES..  N, R, C, IC, IA, JA, A, B,
C                          IL, JL, IJL, LMAX, IU, JU, IJU, UMAX
C       OUTPUT VARIABLES.. Z, L, D, U, FLAG
C
C       PARAMETERS USED INTERNALLY..
C NIA   - IRL,  - VECTORS USED TO FIND THE ROWS OF  L.  AT THE KTH STEP
C NIA   - JRL       OF THE FACTORIZATION,  JRL(K)  POINTS TO THE HEAD 
C       -           OF A LINKED LIST IN  JRL  OF COLUMN INDICES J
C       -           SUCH J .LT. K AND  L(K,J)  IS NONZERO.  ZERO
C       -           INDICATES THE END OF THE LIST.  IRL(J)  (J.LT.K)
C       -           POINTS TO THE SMALLEST I SUCH THAT I .GE. K AND
C       -           L(I,J)  IS NONZERO. 
C       -           SIZE OF EACH = N.
C FIA   - ROW   - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.
C       -           SIZE = N. 
C FIA   - TMP   - HOLDS NEW RIGHT-HAND SIDE  B*  FOR SOLUTION OF THE
C       -           EQUATION UX = B*.
C       -           SIZE = N. 
C
C  INTERNAL VARIABLES..
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO 
C      BE EXAMINED. 
C    SUM - USED IN CALCULATING  TMP.
C
      INTEGER RK,UMAX
      INTEGER  R(1), C(1), IC(1), IA(1), JA(1), IL(1), JL(1), IJL(1)
      INTEGER  IU(1), JU(1), IJU(1), IRL(1), JRL(1), FLAG
      DOUBLE PRECISION  A(1), L(1), D(1), U(1), Z(1), B(1), ROW(1)
      DOUBLE PRECISION  TMP(1), LKI, SUM, DK
C
C  ******  INITIALIZE POINTERS AND TEST STORAGE  ***********************
      IF(IL(N+1)-1 .GT. LMAX) GO TO 104 
      IF(IU(N+1)-1 .GT. UMAX) GO TO 107 
      DO 1 K=1,N
        IRL(K) = IL(K)
        JRL(K) = 0
   1    CONTINUE
C
C  ******  FOR EACH ROW  ***********************************************
      DO 19 K=1,N
C  ******  REVERSE JRL AND ZERO ROW WHERE KTH ROW OF L WILL FILL IN  ***
        ROW(K) = 0
        I1 = 0
        IF (JRL(K) .EQ. 0) GO TO 3
        I = JRL(K)
   2    I2 = JRL(I) 
        JRL(I) = I1 
        I1 = I
        ROW(I) = 0
        I = I2
        IF (I .NE. 0) GO TO 2 
C  ******  SET ROW TO ZERO WHERE U WILL FILL IN  ***********************
   3    JMIN = IJU(K)
        JMAX = JMIN + IU(K+1) - IU(K) - 1
        IF (JMIN .GT. JMAX) GO TO 5
        DO 4 J=JMIN,JMAX
   4      ROW(JU(J)) = 0
C  ******  PLACE KTH ROW OF A IN ROW  **********************************
   5    RK = R(K)
        JMIN = IA(RK)
        JMAX = IA(RK+1) - 1
        DO 6 J=JMIN,JMAX
          ROW(IC(JA(J))) = A(J)
   6      CONTINUE
C  ******  INITIALIZE SUM, AND LINK THROUGH JRL  ***********************
        SUM = B(RK) 
        I = I1
        IF (I .EQ. 0) GO TO 10
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW, SUM  ****************
   7      LKI = -ROW(I)
C  ******  IF L IS NOT REQUIRED, THEN COMMENT OUT THE FOLLOWING LINE  **
          L(IRL(I)) = -LKI
          SUM = SUM + LKI * TMP(I)
          JMIN = IU(I)
          JMAX = IU(I+1) - 1
          IF (JMIN .GT. JMAX) GO TO 9
          MU = IJU(I) - JMIN
          DO 8 J=JMIN,JMAX
   8        ROW(JU(MU+J)) = ROW(JU(MU+J)) + LKI * U(J)
   9      I = JRL(I)
          IF (I .NE. 0) GO TO 7
C
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  *************
  10    IF (ROW(K) .EQ. 0.0D0) GO TO 108
        DK = 1.0D0 / ROW(K)
        D(K) = DK
        TMP(K) = SUM * DK
        IF (K .EQ. N) GO TO 19
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 12
        MU = IJU(K) - JMIN
        DO 11 J=JMIN,JMAX
  11      U(J) = ROW(JU(MU+J)) * DK
  12    CONTINUE
C
C  ******  UPDATE IRL AND JRL, KEEPING JRL IN DECREASING ORDER  ********
        I = I1
        IF (I .EQ. 0) GO TO 18
  14    IRL(I) = IRL(I) + 1
        I1 = JRL(I) 
        IF (IRL(I) .GE. IL(I+1)) GO TO 17
        IJLB = IRL(I) - IL(I) + IJL(I)
        J = JL(IJLB)
  15    IF (I .GT. JRL(J)) GO TO 16
          J = JRL(J)
          GO TO 15
  16    JRL(I) = JRL(J)
        JRL(J) = I
  17    I = I1
        IF (I .NE. 0) GO TO 14
  18    IF (IRL(K) .GE. IL(K+1)) GO TO 19
        J = JL(IJL(K))
        JRL(K) = JRL(J)
        JRL(J) = K
  19    CONTINUE
C
C  ******  SOLVE  UX = TMP  BY BACK SUBSTITUTION  **********************
      K = N
      DO 22 I=1,N
        SUM =  TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 21
        MU = IJU(K) - JMIN
        DO 20 J=JMIN,JMAX
  20      SUM = SUM - U(J) * TMP(JU(MU+J))
  21    TMP(K) =  SUM
        Z(C(K)) =  SUM
  22    K = K-1
      FLAG = 0
      RETURN
C
C ** ERROR.. INSUFFICIENT STORAGE FOR L 
 104  FLAG = 4*N + 1
      RETURN
C ** ERROR.. INSUFFICIENT STORAGE FOR U 
 107  FLAG = 7*N + 1
      RETURN
C ** ERROR.. ZERO PIVOT
 108  FLAG = 8*N + K
      RETURN
      END 
