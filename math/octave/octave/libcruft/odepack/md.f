      SUBROUTINE MD 
     *     (N, IA,JA, MAX, V,L, HEAD,LAST,NEXT, MARK, FLAG) 
CLLL. OPTIMIZE
C***********************************************************************
C  MD -- MINIMUM DEGREE ALGORITHM (BASED ON ELEMENT MODEL)
C***********************************************************************
C
C  DESCRIPTION
C
C    MD FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A
C    GENERAL SPARSE MATRIX M STORED IN (IA,JA,A) FORMAT.
C    WHEN THE STRUCTURE OF M IS NONSYMMETRIC, THE ORDERING IS THAT
C    OBTAINED FOR THE SYMMETRIC MATRIX  M + M-TRANSPOSE.
C
C
C  ADDITIONAL PARAMETERS
C
C    MAX  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS V AND L. 
C           MAX MUST BE AT LEAST  N+2K,  WHERE K IS THE NUMBER OF
C           NONZEROES IN THE STRICT UPPER TRIANGLE OF M + M-TRANSPOSE 
C
C    V    - INTEGER ONE-DIMENSIONAL WORK ARRAY.  DIMENSION = MAX
C
C    L    - INTEGER ONE-DIMENSIONAL WORK ARRAY.  DIMENSION = MAX
C
C    HEAD - INTEGER ONE-DIMENSIONAL WORK ARRAY.  DIMENSION = N
C
C    LAST - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION
C           OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM 
C           DEGREE ORDERING.  DIMENSION = N
C
C    NEXT - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF
C           THE PERMUTATION RETURNED IN LAST.  DIMENSION = N
C
C    MARK - INTEGER ONE-DIMENSIONAL WORK ARRAY (MAY BE THE SAME AS V).
C           DIMENSION = N
C
C    FLAG - INTEGER ERROR FLAG.  VALUES AND THEIR MEANINGS ARE -
C             0     NO ERRORS DETECTED
C             9N+K  INSUFFICIENT STORAGE IN MD
C
C
C  DEFINITIONS OF INTERNAL PARAMETERS
C
C    ---------+---------------------------------------------------------
C    V(S)     - VALUE FIELD OF LIST ENTRY
C    ---------+---------------------------------------------------------
C    L(S)     - LINK FIELD OF LIST ENTRY  (0 =) END OF LIST)
C    ---------+---------------------------------------------------------
C    L(VI)    - POINTER TO ELEMENT LIST OF UNELIMINATED VERTEX VI
C    ---------+---------------------------------------------------------
C    L(EJ)    - POINTER TO BOUNDARY LIST OF ACTIVE ELEMENT EJ
C    ---------+---------------------------------------------------------
C    HEAD(D)  - VJ =) VJ HEAD OF D-LIST D
C             -  0 =) NO VERTEX IN D-LIST D
C
C
C             -                  VI UNELIMINATED VERTEX
C             -          VI IN EK           -       VI NOT IN EK
C    ---------+-----------------------------+---------------------------
C    NEXT(VI) - UNDEFINED BUT NONNEGATIVE   - VJ =) VJ NEXT IN D-LIST 
C             -                             -  0 =) VI TAIL OF D-LIST 
C    ---------+-----------------------------+---------------------------
C    LAST(VI) - (NOT SET UNTIL MDP)         - -D =) VI HEAD OF D-LIST D
C             --VK =) COMPUTE DEGREE        - VJ =) VJ LAST IN D-LIST 
C             - EJ =) VI PROTOTYPE OF EJ    -  0 =) VI NOT IN ANY D-LIST
C             -  0 =) DO NOT COMPUTE DEGREE -
C    ---------+-----------------------------+---------------------------
C    MARK(VI) - MARK(VK)                    - NONNEG. TAG .LT. MARK(VK)
C
C
C             -                   VI ELIMINATED VERTEX
C             -      EI ACTIVE ELEMENT      -           OTHERWISE
C    ---------+-----------------------------+---------------------------
C    NEXT(VI) - -J =) VI WAS J-TH VERTEX    - -J =) VI WAS J-TH VERTEX
C             -       TO BE ELIMINATED      -       TO BE ELIMINATED
C    ---------+-----------------------------+---------------------------
C    LAST(VI) -  M =) SIZE OF EI = M        - UNDEFINED
C    ---------+-----------------------------+---------------------------
C    MARK(VI) - -M =) OVERLAP COUNT OF EI   - UNDEFINED
C             -       WITH EK = M           -
C             - OTHERWISE NONNEGATIVE TAG   -
C             -       .LT. MARK(VK)         -
C
C-----------------------------------------------------------------------
C
      INTEGER  IA(1), JA(1),  V(1), L(1),  HEAD(1), LAST(1), NEXT(1), 
     *   MARK(1),  FLAG,  TAG, DMIN, VK,EK, TAIL
      EQUIVALENCE  (VK,EK)
C
C----INITIALIZATION 
      TAG = 0
      CALL  MDI
     *   (N, IA,JA, MAX,V,L, HEAD,LAST,NEXT, MARK,TAG, FLAG)
      IF (FLAG.NE.0)  RETURN
C
      K = 0
      DMIN = 1
C
C----WHILE  K .LT. N  DO
   1  IF (K.GE.N)  GO TO 4
C
C------SEARCH FOR VERTEX OF MINIMUM DEGREE
   2    IF (HEAD(DMIN).GT.0)  GO TO 3
          DMIN = DMIN + 1
          GO TO 2
C
C------REMOVE VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST
   3    VK = HEAD(DMIN)
        HEAD(DMIN) = NEXT(VK) 
        IF (HEAD(DMIN).GT.0)  LAST(HEAD(DMIN)) = -DMIN
C
C------NUMBER VERTEX VK, ADJUST TAG, AND TAG VK
        K = K+1
        NEXT(VK) = -K
        LAST(EK) = DMIN - 1
        TAG = TAG + LAST(EK)
        MARK(VK) = TAG
C
C------FORM ELEMENT EK FROM UNELIMINATED NEIGHBORS OF VK
        CALL  MDM
     *     (VK,TAIL, V,L, LAST,NEXT, MARK)
C
C------PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION
        CALL  MDP
     *     (K,EK,TAIL, V,L, HEAD,LAST,NEXT, MARK) 
C
C------UPDATE DEGREES OF UNELIMINATED VERTICES IN EK
        CALL  MDU
     *     (EK,DMIN, V,L, HEAD,LAST,NEXT, MARK)
C
        GO TO 1
C
C----GENERATE INVERSE PERMUTATION FROM PERMUTATION
   4  DO 5 K=1,N
        NEXT(K) = -NEXT(K)
   5    LAST(NEXT(K)) = K
C
      RETURN
      END 
