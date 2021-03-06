      SUBROUTINE CDRV
     *     (N, R,C,IC, IA,JA,A, B, Z, NSP,ISP,RSP,ESP, PATH, FLAG)
CLLL. OPTIMIZE
C*** SUBROUTINE CDRV
C*** DRIVER FOR SUBROUTINES FOR SOLVING SPARSE NONSYMMETRIC SYSTEMS OF
C       LINEAR EQUATIONS (COMPRESSED POINTER STORAGE)
C
C
C    PARAMETERS
C    CLASS ABBREVIATIONS ARE--
C       N - INTEGER VARIABLE
C       F - REAL VARIABLE
C       V - SUPPLIES A VALUE TO THE DRIVER
C       R - RETURNS A RESULT FROM THE DRIVER
C       I - USED INTERNALLY BY THE DRIVER
C       A - ARRAY
C
C CLASS - PARAMETER 
C ------+---------- 
C       - 
C         THE NONZERO ENTRIES OF THE COEFFICIENT MATRIX M ARE STORED
C    ROW-BY-ROW IN THE ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO
C    ENTRIES IN EACH ROW, WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY
C    LIES.  THE COLUMN INDICES WHICH CORRESPOND TO THE NONZERO ENTRIES
C    OF M ARE STORED IN THE ARRAY JA.  I.E., IF  A(K) = M(I,J),  THEN 
C    JA(K) = J.  IN ADDITION, WE NEED TO KNOW WHERE EACH ROW STARTS AND
C    HOW LONG IT IS.  THE INDEX POSITIONS IN JA AND A WHERE THE ROWS OF
C    M BEGIN ARE STORED IN THE ARRAY IA.  I.E., IF M(I,J) IS THE FIRST
C    NONZERO ENTRY (STORED) IN THE I-TH ROW AND A(K) = M(I,J),  THEN
C    IA(I) = K.  MOREOVER, THE INDEX IN JA AND A OF THE FIRST LOCATION
C    FOLLOWING THE LAST ELEMENT IN THE LAST ROW IS STORED IN IA(N+1). 
C    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS GIVEN BY
C    IA(I+1) - IA(I),  THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED 
C    CONSECUTIVELY IN
C            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),
C    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN 
C            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).
C    FOR EXAMPLE, THE 5 BY 5 MATRIX
C                ( 1. 0. 2. 0. 0.)
C                ( 0. 3. 0. 0. 0.)
C            M = ( 0. 4. 5. 6. 0.)
C                ( 0. 0. 0. 7. 0.)
C                ( 0. 0. 0. 8. 9.)
C    WOULD BE STORED AS
C               - 1  2  3  4  5  6  7  8  9
C            ---+--------------------------
C            IA - 1  3  4  7  8 10
C            JA - 1  3  2  2  3  4  4  4  5
C             A - 1. 2. 3. 4. 5. 6. 7. 8. 9.         .
C
C NV    - N     - NUMBER OF VARIABLES/EQUATIONS.
C FVA   - A     - NONZERO ENTRIES OF THE COEFFICIENT MATRIX M, STORED 
C       -           BY ROWS.
C       -           SIZE = NUMBER OF NONZERO ENTRIES IN M.
C NVA   - IA    - POINTERS TO DELIMIT THE ROWS IN A.
C       -           SIZE = N+1.
C NVA   - JA    - COLUMN NUMBERS CORRESPONDING TO THE ELEMENTS OF A.
C       -           SIZE = SIZE OF A.
C FVA   - B     - RIGHT-HAND SIDE B.  B AND Z CAN THE SAME ARRAY.
C       -           SIZE = N. 
C FRA   - Z     - SOLUTION X.  B AND Z CAN BE THE SAME ARRAY.
C       -           SIZE = N. 
C
C         THE ROWS AND COLUMNS OF THE ORIGINAL MATRIX M CAN BE
C    REORDERED (E.G., TO REDUCE FILLIN OR ENSURE NUMERICAL STABILITY) 
C    BEFORE CALLING THE DRIVER.  IF NO REORDERING IS DONE, THEN SET
C    R(I) = C(I) = IC(I) = I  FOR I=1,...,N.  THE SOLUTION Z IS RETURNED
C    IN THE ORIGINAL ORDER.
C         IF THE COLUMNS HAVE BEEN REORDERED (I.E.,  C(I).NE.I  FOR SOME
C    I), THEN THE DRIVER WILL CALL A SUBROUTINE (NROC) WHICH REARRANGES
C    EACH ROW OF JA AND A, LEAVING THE ROWS IN THE ORIGINAL ORDER, BUT
C    PLACING THE ELEMENTS OF EACH ROW IN INCREASING ORDER WITH RESPECT
C    TO THE NEW ORDERING.  IF  PATH.NE.1,  THEN NROC IS ASSUMED TO HAVE
C    BEEN CALLED ALREADY.
C
C NVA   - R     - ORDERING OF THE ROWS OF M.
C       -           SIZE = N. 
C NVA   - C     - ORDERING OF THE COLUMNS OF M.
C       -           SIZE = N. 
C NVA   - IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF M.  I.E., 
C       -           IC(C(I)) = I  FOR I=1,...,N.
C       -           SIZE = N. 
C
C         THE SOLUTION OF THE SYSTEM OF LINEAR EQUATIONS IS DIVIDED INTO
C    THREE STAGES --
C      NSFC -- THE MATRIX M IS PROCESSED SYMBOLICALLY TO DETERMINE WHERE
C               FILLIN WILL OCCUR DURING THE NUMERIC FACTORIZATION.
C      NNFC -- THE MATRIX M IS FACTORED NUMERICALLY INTO THE PRODUCT LDU
C               OF A UNIT LOWER TRIANGULAR MATRIX L, A DIAGONAL MATRIX
C               D, AND A UNIT UPPER TRIANGULAR MATRIX U, AND THE SYSTEM
C               MX = B  IS SOLVED.
C      NNSC -- THE LINEAR SYSTEM  MX = B  IS SOLVED USING THE LDU
C  OR           FACTORIZATION FROM NNFC.
C      NNTC -- THE TRANSPOSED LINEAR SYSTEM  MT X = B  IS SOLVED USING
C               THE LDU FACTORIZATION FROM NNF.
C    FOR SEVERAL SYSTEMS WHOSE COEFFICIENT MATRICES HAVE THE SAME
C    NONZERO STRUCTURE, NSFC NEED BE DONE ONLY ONCE (FOR THE FIRST
C    SYSTEM).  THEN NNFC IS DONE ONCE FOR EACH ADDITIONAL SYSTEM.  FOR
C    SEVERAL SYSTEMS WITH THE SAME COEFFICIENT MATRIX, NSFC AND NNFC
C    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM).  THEN NNSC OR NNTC
C    IS DONE ONCE FOR EACH ADDITIONAL RIGHT-HAND SIDE.
C
C NV    - PATH  - PATH SPECIFICATION.  VALUES AND THEIR MEANINGS ARE --
C       -           1  PERFORM NROC, NSFC, AND NNFC.
C       -           2  PERFORM NNFC ONLY  (NSFC IS ASSUMED TO HAVE BEEN
C       -               DONE IN A MANNER COMPATIBLE WITH THE STORAGE
C       -               ALLOCATION USED IN THE DRIVER).
C       -           3  PERFORM NNSC ONLY  (NSFC AND NNFC ARE ASSUMED TO
C       -               HAVE BEEN DONE IN A MANNER COMPATIBLE WITH THE
C       -               STORAGE ALLOCATION USED IN THE DRIVER).
C       -           4  PERFORM NNTC ONLY  (NSFC AND NNFC ARE ASSUMED TO
C       -               HAVE BEEN DONE IN A MANNER COMPATIBLE WITH THE
C       -               STORAGE ALLOCATION USED IN THE DRIVER).
C       -           5  PERFORM NROC AND NSFC.
C
C         VARIOUS ERRORS ARE DETECTED BY THE DRIVER AND THE INDIVIDUAL
C    SUBROUTINES.
C
C NR    - FLAG  - ERROR FLAG.  VALUES AND THEIR MEANINGS ARE --
C       -             0     NO ERRORS DETECTED
C       -             N+K   NULL ROW IN A  --  ROW = K
C       -            2N+K   DUPLICATE ENTRY IN A  --  ROW = K
C       -            3N+K   INSUFFICIENT STORAGE IN NSFC  --  ROW = K 
C       -            4N+1   INSUFFICIENT STORAGE IN NNFC
C       -            5N+K   NULL PIVOT  --  ROW = K
C       -            6N+K   INSUFFICIENT STORAGE IN NSFC  --  ROW = K 
C       -            7N+1   INSUFFICIENT STORAGE IN NNFC
C       -            8N+K   ZERO PIVOT  --  ROW = K
C       -           10N+1   INSUFFICIENT STORAGE IN CDRV
C       -           11N+1   ILLEGAL PATH SPECIFICATION
C
C         WORKING STORAGE IS NEEDED FOR THE FACTORED FORM OF THE MATRIX
C    M PLUS VARIOUS TEMPORARY VECTORS.  THE ARRAYS ISP AND RSP SHOULD BE
C    EQUIVALENCED.  INTEGER STORAGE IS ALLOCATED FROM THE BEGINNING OF
C    ISP AND REAL STORAGE FROM THE END OF RSP.
C
C NV    - NSP   - DECLARED DIMENSION OF RSP.  NSP GENERALLY MUST
C       -           BE LARGER THAN  8N+2 + 2K  (WHERE  K = (NUMBER OF 
C       -           NONZERO ENTRIES IN M)).
C NVIRA - ISP   - INTEGER WORKING STORAGE DIVIDED UP INTO VARIOUS ARRAYS
C       -           NEEDED BY THE SUBROUTINES.  ISP AND RSP SHOULD BE 
C       -           EQUIVALENCED.
C       -           SIZE = LRATIO*NSP.
C FVIRA - RSP   - REAL WORKING STORAGE DIVIDED UP INTO VARIOUS ARRAYS 
C       -           NEEDED BY THE SUBROUTINES.  ISP AND RSP SHOULD BE 
C       -           EQUIVALENCED.
C       -           SIZE = NSP.
C NR    - ESP   - IF SUFFICIENT STORAGE WAS AVAILABLE TO PERFORM THE
C       -           SYMBOLIC FACTORIZATION (NSFC), THEN ESP IS SET TO 
C       -           THE AMOUNT OF EXCESS STORAGE PROVIDED (NEGATIVE IF
C       -           INSUFFICIENT STORAGE WAS AVAILABLE TO PERFORM THE 
C       -           NUMERIC FACTORIZATION (NNFC)).
C
C
C  CONVERSION TO DOUBLE PRECISION
C
C    TO CONVERT THESE ROUTINES FOR DOUBLE PRECISION ARRAYS..
C    (1) USE THE DOUBLE PRECISION DECLARATIONS IN PLACE OF THE REAL
C    DECLARATIONS IN EACH SUBPROGRAM, AS GIVEN IN COMMENT CARDS.
C    (2) CHANGE THE DATA-LOADED VALUE OF THE INTEGER  LRATIO
C    IN SUBROUTINE CDRV, AS INDICATED BELOW.
C    (3) CHANGE E0 TO D0 IN THE CONSTANTS IN STATEMENT NUMBER 10
C    IN SUBROUTINE NNFC AND THE LINE FOLLOWING THAT.
C
      INTEGER  R(1), C(1), IC(1),  IA(1), JA(1),  ISP(1), ESP,  PATH, 
     *   FLAG,  D, U, Q, ROW, TMP, AR,  UMAX
      DOUBLE PRECISION  A(1), B(1), Z(1), RSP(1)
C
C  SET LRATIO EQUAL TO THE RATIO BETWEEN THE LENGTH OF FLOATING POINT 
C  AND INTEGER ARRAY DATA.  E. G., LRATIO = 1 FOR (REAL, INTEGER),
C  LRATIO = 2 FOR (DOUBLE PRECISION, INTEGER)
C
      DATA LRATIO/2/
C
      IF (PATH.LT.1 .OR. 5.LT.PATH)  GO TO 111
C******INITIALIZE AND DIVIDE UP TEMPORARY STORAGE  *******************
      IL   = 1
      IJL  = IL  + (N+1)
      IU   = IJL +   N
      IJU  = IU  + (N+1)
      IRL  = IJU +   N
      JRL  = IRL +   N
      JL   = JRL +   N
C
C  ******  REORDER A IF NECESSARY, CALL NSFC IF FLAG IS SET  ***********
      IF ((PATH-1) * (PATH-5) .NE. 0)  GO TO 5
        MAX = (LRATIO*NSP + 1 - JL) - (N+1) - 5*N 
        JLMAX = MAX/2
        Q     = JL   + JLMAX
        IRA   = Q    + (N+1)
        JRA   = IRA  +   N
        IRAC  = JRA  +   N
        IRU   = IRAC +   N
        JRU   = IRU  +   N
        JUTMP = JRU  +   N
        JUMAX = LRATIO*NSP  + 1 - JUTMP 
        ESP = MAX/LRATIO
        IF (JLMAX.LE.0 .OR. JUMAX.LE.0)  GO TO 110
C
        DO 1 I=1,N
          IF (C(I).NE.I)  GO TO 2
   1      CONTINUE
        GO TO 3
   2    AR = NSP + 1 - N
        CALL  NROC
     *     (N, IC, IA,JA,A, ISP(IL), RSP(AR), ISP(IU), FLAG)
        IF (FLAG.NE.0)  GO TO 100
C
   3    CALL  NSFC
     *     (N, R, IC, IA,JA,
     *      JLMAX, ISP(IL), ISP(JL), ISP(IJL),
     *      JUMAX, ISP(IU), ISP(JUTMP), ISP(IJU), 
     *      ISP(Q), ISP(IRA), ISP(JRA), ISP(IRAC),
     *      ISP(IRL), ISP(JRL), ISP(IRU), ISP(JRU),  FLAG)
        IF(FLAG .NE. 0)  GO TO 100
C  ******  MOVE JU NEXT TO JL  *****************************************
        JLMAX = ISP(IJL+N-1)
        JU    = JL + JLMAX
        JUMAX = ISP(IJU+N-1)
        IF (JUMAX.LE.0)  GO TO 5
        DO 4 J=1,JUMAX
   4      ISP(JU+J-1) = ISP(JUTMP+J-1)
C
C  ******  CALL REMAINING SUBROUTINES  *********************************
   5  JLMAX = ISP(IJL+N-1)
      JU    = JL  + JLMAX
      JUMAX = ISP(IJU+N-1)
      L     = (JU + JUMAX - 2 + LRATIO)  /  LRATIO    +    1
      LMAX  = ISP(IL+N) - 1
      D     = L   + LMAX
      U     = D   + N
      ROW   = NSP + 1 - N
      TMP   = ROW - N
      UMAX  = TMP - U
      ESP   = UMAX - (ISP(IU+N) - 1)
C
      IF ((PATH-1) * (PATH-2) .NE. 0)  GO TO 6
        IF (UMAX.LT.0)  GO TO 110
        CALL NNFC
     *     (N,  R, C, IC,  IA, JA, A, Z, B,
     *      LMAX, ISP(IL), ISP(JL), ISP(IJL), RSP(L),  RSP(D),
     *      UMAX, ISP(IU), ISP(JU), ISP(IJU), RSP(U),
     *      RSP(ROW), RSP(TMP),  ISP(IRL), ISP(JRL),  FLAG) 
        IF(FLAG .NE. 0)  GO TO 100
C
   6  IF ((PATH-3) .NE. 0)  GO TO 7
        CALL NNSC
     *     (N,  R, C,  ISP(IL), ISP(JL), ISP(IJL), RSP(L),
     *      RSP(D),    ISP(IU), ISP(JU), ISP(IJU), RSP(U),
     *      Z, B,  RSP(TMP))
C
   7  IF ((PATH-4) .NE. 0)  GO TO 8
        CALL NNTC
     *     (N,  R, C,  ISP(IL), ISP(JL), ISP(IJL), RSP(L),
     *      RSP(D),    ISP(IU), ISP(JU), ISP(IJU), RSP(U),
     *      Z, B,  RSP(TMP))
   8  RETURN
C
C ** ERROR.. ERROR DETECTED IN NROC, NSFC, NNFC, OR NNSC
 100  RETURN
C ** ERROR.. INSUFFICIENT STORAGE
 110  FLAG = 10*N + 1
      RETURN
C ** ERROR.. ILLEGAL PATH SPECIFICATION 
 111  FLAG = 11*N + 1
      RETURN
      END 
