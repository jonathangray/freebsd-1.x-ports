      SUBROUTINE MDI
     *     (N, IA,JA, MAX,V,L, HEAD,LAST,NEXT, MARK,TAG, FLAG)
CLLL. OPTIMIZE
C***********************************************************************
C  MDI -- INITIALIZATION
C***********************************************************************
      INTEGER  IA(1), JA(1),  V(1), L(1),  HEAD(1), LAST(1), NEXT(1), 
     *   MARK(1), TAG,  FLAG,  SFS, VI,DVI, VJ
C
C----INITIALIZE DEGREES, ELEMENT LISTS, AND DEGREE LISTS
      DO 1 VI=1,N
        MARK(VI) = 1
        L(VI) = 0
   1    HEAD(VI) = 0
      SFS = N+1
C
C----CREATE NONZERO STRUCTURE 
C----FOR EACH NONZERO ENTRY A(VI,VJ)
      DO 6 VI=1,N
        JMIN = IA(VI)
        JMAX = IA(VI+1) - 1
        IF (JMIN.GT.JMAX)  GO TO 6
        DO 5 J=JMIN,JMAX
          VJ = JA(J)
          IF (VJ-VI) 2, 5, 4
C
C------IF A(VI,VJ) IS IN STRICT LOWER TRIANGLE
C------CHECK FOR PREVIOUS OCCURRENCE OF A(VJ,VI)
   2      LVK = VI
          KMAX = MARK(VI) - 1 
          IF (KMAX .EQ. 0) GO TO 4
          DO 3 K=1,KMAX
            LVK = L(LVK)
            IF (V(LVK).EQ.VJ) GO TO 5
   3        CONTINUE
C----FOR UNENTERED ENTRIES A(VI,VJ)
   4        IF (SFS.GE.MAX)  GO TO 101
C
C------ENTER VJ IN ELEMENT LIST FOR VI
            MARK(VI) = MARK(VI) + 1
            V(SFS) = VJ
            L(SFS) = L(VI)
            L(VI) = SFS
            SFS = SFS+1
C
C------ENTER VI IN ELEMENT LIST FOR VJ
            MARK(VJ) = MARK(VJ) + 1
            V(SFS) = VI
            L(SFS) = L(VJ)
            L(VJ) = SFS
            SFS = SFS+1
   5      CONTINUE
   6    CONTINUE
C
C----CREATE DEGREE LISTS AND INITIALIZE MARK VECTOR
      DO 7 VI=1,N
        DVI = MARK(VI)
        NEXT(VI) = HEAD(DVI)
        HEAD(DVI) = VI
        LAST(VI) = -DVI
        NEXTVI = NEXT(VI)
        IF (NEXTVI.GT.0)  LAST(NEXTVI) = VI
   7    MARK(VI) = TAG
C
      RETURN
C
C ** ERROR-  INSUFFICIENT STORAGE
 101  FLAG = 9*N + VI
      RETURN
      END 
