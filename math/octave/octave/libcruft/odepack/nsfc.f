      SUBROUTINE NSFC
     *      (N, R, IC, IA,JA, JLMAX,IL,JL,IJL, JUMAX,IU,JU,IJU,
     *       Q, IRA,JRA, IRAC, IRL,JRL, IRU,JRU, FLAG)
CLLL. OPTIMIZE
C*** SUBROUTINE NSFC
C*** SYMBOLIC LDU-FACTORIZATION OF NONSYMMETRIC SPARSE MATRIX
C      (COMPRESSED POINTER STORAGE)
C
C
C       INPUT VARIABLES.. N, R, IC, IA, JA, JLMAX, JUMAX.
C       OUTPUT VARIABLES.. IL, JL, IJL, IU, JU, IJU, FLAG.
C
C       PARAMETERS USED INTERNALLY..
C NIA   - Q     - SUPPOSE  M*  IS THE RESULT OF REORDERING  M.  IF
C       -           PROCESSING OF THE ITH ROW OF  M*  (HENCE THE ITH
C       -           ROW OF  U) IS BEING DONE,  Q(J)  IS INITIALLY
C       -           NONZERO IF  M*(I,J) IS NONZERO (J.GE.I).  SINCE
C       -           VALUES NEED NOT BE STORED, EACH ENTRY POINTS TO THE
C       -           NEXT NONZERO AND  Q(N+1)  POINTS TO THE FIRST.  N+1
C       -           INDICATES THE END OF THE LIST.  FOR EXAMPLE, IF N=9
C       -           AND THE 5TH ROW OF  M*  IS
C       -              0 X X 0 X 0 0 X 0
C       -           THEN  Q  WILL INITIALLY BE
C       -              A A A A 8 A A 10 5           (A - ARBITRARY).
C       -           AS THE ALGORITHM PROCEEDS, OTHER ELEMENTS OF  Q
C       -           ARE INSERTED IN THE LIST BECAUSE OF FILLIN.
C       -           Q  IS USED IN AN ANALOGOUS MANNER TO COMPUTE THE
C       -           ITH COLUMN OF  L.
C       -           SIZE = N+1.
C NIA   - IRA,  - VECTORS USED TO FIND THE COLUMNS OF  M.  AT THE KTH 
C NIA   - JRA,      STEP OF THE FACTORIZATION,  IRAC(K)  POINTS TO THE
C NIA   - IRAC      HEAD OF A LINKED LIST IN  JRA  OF ROW INDICES I
C       -           SUCH THAT I .GE. K AND  M(I,K)  IS NONZERO.  ZERO 
C       -           INDICATES THE END OF THE LIST.  IRA(I)  (I.GE.K)
C       -           POINTS TO THE SMALLEST J SUCH THAT J .GE. K AND
C       -           M(I,J)  IS NONZERO. 
C       -           SIZE OF EACH = N.
C NIA   - IRL,  - VECTORS USED TO FIND THE ROWS OF  L.  AT THE KTH STEP
C NIA   - JRL       OF THE FACTORIZATION,  JRL(K)  POINTS TO THE HEAD 
C       -           OF A LINKED LIST IN  JRL  OF COLUMN INDICES J
C       -           SUCH J .LT. K AND  L(K,J)  IS NONZERO.  ZERO
C       -           INDICATES THE END OF THE LIST.  IRL(J)  (J.LT.K)
C       -           POINTS TO THE SMALLEST I SUCH THAT I .GE. K AND
C       -           L(I,J)  IS NONZERO. 
C       -           SIZE OF EACH = N.
C NIA   - IRU,  - VECTORS USED IN A MANNER ANALOGOUS TO  IRL AND JRL
C NIA   - JRU       TO FIND THE COLUMNS OF  U.
C       -           SIZE OF EACH = N.
C
C  INTERNAL VARIABLES..
C    JLPTR - POINTS TO THE LAST POSITION USED IN  JL.
C    JUPTR - POINTS TO THE LAST POSITION USED IN  JU.
C    JMIN,JMAX - ARE THE INDICES IN  A OR U  OF THE FIRST AND LAST
C                ELEMENTS TO BE EXAMINED IN A GIVEN ROW.
C                FOR EXAMPLE,  JMIN=IA(K), JMAX=IA(K+1)-1.
C
      INTEGER CEND, QM, REND, RK, VJ
      INTEGER IA(1), JA(1), IRA(1), JRA(1), IL(1), JL(1), IJL(1)
      INTEGER IU(1), JU(1), IJU(1), IRL(1), JRL(1), IRU(1), JRU(1)
      INTEGER R(1), IC(1), Q(1), IRAC(1), FLAG
C
C  ******  INITIALIZE POINTERS  ****************************************
      NP1 = N + 1
      JLMIN = 1
      JLPTR = 0
      IL(1) = 1
      JUMIN = 1
      JUPTR = 0
      IU(1) = 1
      DO 1 K=1,N
        IRAC(K) = 0 
        JRA(K) = 0
        JRL(K) = 0
   1    JRU(K) = 0
C  ******  INITIALIZE COLUMN POINTERS FOR A  ***************************
      DO 2 K=1,N
        RK = R(K)
        IAK = IA(RK)
        IF (IAK .GE. IA(RK+1))  GO TO 101
        JAIAK = IC(JA(IAK))
        IF (JAIAK .GT. K)  GO TO 105
        JRA(K) = IRAC(JAIAK)
        IRAC(JAIAK) = K
   2    IRA(K) = IAK
C
C  ******  FOR EACH COLUMN OF L AND ROW OF U  **************************
      DO 41 K=1,N
C
C  ******  INITIALIZE Q FOR COMPUTING KTH COLUMN OF L  *****************
        Q(NP1) = NP1
        LUK = -1
C  ******  BY FILLING IN KTH COLUMN OF A  ******************************
        VJ = IRAC(K)
        IF (VJ .EQ. 0)  GO TO 5
   3      QM = NP1
   4      M = QM
          QM =  Q(M)
          IF (QM .LT. VJ)  GO TO 4
          IF (QM .EQ. VJ)  GO TO 102
            LUK = LUK + 1
            Q(M) = VJ
            Q(VJ) = QM
            VJ = JRA(VJ)
            IF (VJ .NE. 0)  GO TO 3
C  ******  LINK THROUGH JRU  *******************************************
   5    LASTID = 0
        LASTI = 0
        IJL(K) = JLPTR
        I = K
   6      I = JRU(I)
          IF (I .EQ. 0)  GO TO 10
          QM = NP1
          JMIN = IRL(I)
          JMAX = IJL(I) + IL(I+1) - IL(I) - 1
          LONG = JMAX - JMIN
          IF (LONG .LT. 0)  GO TO 6
          JTMP = JL(JMIN)
          IF (JTMP .NE. K)  LONG = LONG + 1
          IF (JTMP .EQ. K)  R(I) = -R(I)
          IF (LASTID .GE. LONG)  GO TO 7
            LASTI = I
            LASTID = LONG
C  ******  AND MERGE THE CORRESPONDING COLUMNS INTO THE KTH COLUMN  ****
   7      DO 9 J=JMIN,JMAX
            VJ = JL(J)
   8        M = QM
            QM = Q(M)
            IF (QM .LT. VJ)  GO TO 8
            IF (QM .EQ. VJ)  GO TO 9
              LUK = LUK + 1
              Q(M) = VJ
              Q(VJ) = QM
              QM = VJ
   9        CONTINUE
            GO TO 6 
C  ******  LASTI IS THE LONGEST COLUMN MERGED INTO THE KTH  ************
C  ******  SEE IF IT EQUALS THE ENTIRE KTH COLUMN  *********************
  10    QM = Q(NP1) 
        IF (QM .NE. K)  GO TO 105
        IF (LUK .EQ. 0)  GO TO 17
        IF (LASTID .NE. LUK)  GO TO 11
C  ******  IF SO, JL CAN BE COMPRESSED  ********************************
        IRLL = IRL(LASTI)
        IJL(K) = IRLL + 1
        IF (JL(IRLL) .NE. K)  IJL(K) = IJL(K) - 1 
        GO TO 17
C  ******  IF NOT, SEE IF KTH COLUMN CAN OVERLAP THE PREVIOUS ONE  *****
  11    IF (JLMIN .GT. JLPTR)  GO TO 15 
        QM = Q(QM)
        DO 12 J=JLMIN,JLPTR
          IF (JL(J) - QM)  12, 13, 15
  12      CONTINUE
        GO TO 15
  13    IJL(K) = J
        DO 14 I=J,JLPTR
          IF (JL(I) .NE. QM)  GO TO 15
          QM = Q(QM)
          IF (QM .GT. N)  GO TO 17
  14      CONTINUE
        JLPTR = J - 1
C  ******  MOVE COLUMN INDICES FROM Q TO JL, UPDATE VECTORS  ***********
  15    JLMIN = JLPTR + 1
        IJL(K) = JLMIN
        IF (LUK .EQ. 0)  GO TO 17
        JLPTR = JLPTR + LUK
        IF (JLPTR .GT. JLMAX)  GO TO 103
          QM = Q(NP1)
          DO 16 J=JLMIN,JLPTR 
            QM = Q(QM)
  16        JL(J) = QM
  17    IRL(K) = IJL(K)
        IL(K+1) = IL(K) + LUK 
C
C  ******  INITIALIZE Q FOR COMPUTING KTH ROW OF U  ********************
        Q(NP1) = NP1
        LUK = -1
C  ******  BY FILLING IN KTH ROW OF REORDERED A  ***********************
        RK = R(K)
        JMIN = IRA(K)
        JMAX = IA(RK+1) - 1
        IF (JMIN .GT. JMAX)  GO TO 20
        DO 19 J=JMIN,JMAX
          VJ = IC(JA(J))
          QM = NP1
  18      M = QM
          QM = Q(M) 
          IF (QM .LT. VJ)  GO TO 18
          IF (QM .EQ. VJ)  GO TO 102
            LUK = LUK + 1
            Q(M) = VJ
            Q(VJ) = QM
  19      CONTINUE
C  ******  LINK THROUGH JRL,  ******************************************
  20    LASTID = 0
        LASTI = 0
        IJU(K) = JUPTR
        I = K
        I1 = JRL(K) 
  21      I = I1
          IF (I .EQ. 0)  GO TO 26
          I1 = JRL(I)
          QM = NP1
          JMIN = IRU(I)
          JMAX = IJU(I) + IU(I+1) - IU(I) - 1
          LONG = JMAX - JMIN
          IF (LONG .LT. 0)  GO TO 21
          JTMP = JU(JMIN)
          IF (JTMP .EQ. K)  GO TO 22
C  ******  UPDATE IRL AND JRL, *****************************************
            LONG = LONG + 1
            CEND = IJL(I) + IL(I+1) - IL(I)
            IRL(I) = IRL(I) + 1
            IF (IRL(I) .GE. CEND)  GO TO 22
              J = JL(IRL(I))
              JRL(I) = JRL(J) 
              JRL(J) = I
  22      IF (LASTID .GE. LONG)  GO TO 23
            LASTI = I
            LASTID = LONG
C  ******  AND MERGE THE CORRESPONDING ROWS INTO THE KTH ROW  **********
  23      DO 25 J=JMIN,JMAX
            VJ = JU(J)
  24        M = QM
            QM = Q(M)
            IF (QM .LT. VJ)  GO TO 24
            IF (QM .EQ. VJ)  GO TO 25
              LUK = LUK + 1
              Q(M) = VJ
              Q(VJ) = QM
              QM = VJ
  25        CONTINUE
          GO TO 21
C  ******  UPDATE JRL(K) AND IRL(K)  ***********************************
  26    IF (IL(K+1) .LE. IL(K))  GO TO 27
          J = JL(IRL(K))
          JRL(K) = JRL(J)
          JRL(J) = K
C  ******  LASTI IS THE LONGEST ROW MERGED INTO THE KTH  ***************
C  ******  SEE IF IT EQUALS THE ENTIRE KTH ROW  ************************
  27    QM = Q(NP1) 
        IF (QM .NE. K)  GO TO 105
        IF (LUK .EQ. 0)  GO TO 34
        IF (LASTID .NE. LUK)  GO TO 28
C  ******  IF SO, JU CAN BE COMPRESSED  ********************************
        IRUL = IRU(LASTI)
        IJU(K) = IRUL + 1
        IF (JU(IRUL) .NE. K)  IJU(K) = IJU(K) - 1 
        GO TO 34
C  ******  IF NOT, SEE IF KTH ROW CAN OVERLAP THE PREVIOUS ONE  ********
  28    IF (JUMIN .GT. JUPTR)  GO TO 32 
        QM = Q(QM)
        DO 29 J=JUMIN,JUPTR
          IF (JU(J) - QM)  29, 30, 32
  29      CONTINUE
        GO TO 32
  30    IJU(K) = J
        DO 31 I=J,JUPTR
          IF (JU(I) .NE. QM)  GO TO 32
          QM = Q(QM)
          IF (QM .GT. N)  GO TO 34
  31      CONTINUE
        JUPTR = J - 1
C  ******  MOVE ROW INDICES FROM Q TO JU, UPDATE VECTORS  **************
  32    JUMIN = JUPTR + 1
        IJU(K) = JUMIN
        IF (LUK .EQ. 0)  GO TO 34
        JUPTR = JUPTR + LUK
        IF (JUPTR .GT. JUMAX)  GO TO 106
          QM = Q(NP1)
          DO 33 J=JUMIN,JUPTR 
            QM = Q(QM)
  33        JU(J) = QM
  34    IRU(K) = IJU(K)
        IU(K+1) = IU(K) + LUK 
C
C  ******  UPDATE IRU, JRU  ********************************************
        I = K
  35      I1 = JRU(I)
          IF (R(I) .LT. 0)  GO TO 36
          REND = IJU(I) + IU(I+1) - IU(I)
          IF (IRU(I) .GE. REND)  GO TO 37
            J = JU(IRU(I))
            JRU(I) = JRU(J)
            JRU(J) = I
            GO TO 37
  36      R(I) = -R(I)
  37      I = I1
          IF (I .EQ. 0)  GO TO 38
          IRU(I) = IRU(I) + 1 
          GO TO 35
C
C  ******  UPDATE IRA, JRA, IRAC  **************************************
  38    I = IRAC(K) 
        IF (I .EQ. 0)  GO TO 41
  39      I1 = JRA(I)
          IRA(I) = IRA(I) + 1 
          IF (IRA(I) .GE. IA(R(I)+1))  GO TO 40
          IRAI = IRA(I)
          JAIRAI = IC(JA(IRAI))
          IF (JAIRAI .GT. I)  GO TO 40
          JRA(I) = IRAC(JAIRAI)
          IRAC(JAIRAI) = I
  40      I = I1
          IF (I .NE. 0)  GO TO 39
  41    CONTINUE
C
      IJL(N) = JLPTR
      IJU(N) = JUPTR
      FLAG = 0
      RETURN
C
C ** ERROR.. NULL ROW IN A
 101  FLAG = N + RK 
      RETURN
C ** ERROR.. DUPLICATE ENTRY IN A
 102  FLAG = 2*N + RK
      RETURN
C ** ERROR.. INSUFFICIENT STORAGE FOR JL
 103  FLAG = 3*N + K
      RETURN
C ** ERROR.. NULL PIVOT
 105  FLAG = 5*N + K
      RETURN
C ** ERROR.. INSUFFICIENT STORAGE FOR JU
 106  FLAG = 6*N + K
      RETURN
      END 
