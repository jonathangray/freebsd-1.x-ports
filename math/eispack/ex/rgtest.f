C
C     THIS DRIVER TESTS  EISPACK  FOR THE CLASS OF REAL GENERAL
C     MATRICES SUMMARIZING THE FIGURES OF MERIT FOR ALL PATHS.
C
C     THIS DRIVER IS CATALOGUED AS  EISPDRV4(RGSUMARY).
C
C     THE DIMENSION OF  A,Z,RM1, AND  ASAVE  SHOULD BE  NM  BY  NM.
C     THE DIMENSION OF  WR,WI,WR1,WI1,SELECT,SLHOLD,INT,SCALE,ORT,RV1,
C     AND  RV2  SHOULD BE  NM.
C     THE DIMENSION OF  AHOLD  SHOULD BE  NM  BY  NM.
C     HERE NM = 20.
C
      DOUBLE PRECISION A(20,20),Z(20,20),ASAVE(20,20),RM1(20,20),
     X        AHOLD( 20, 20),ORT( 20),WR( 20),WI(20),
     X        WR1( 20),WI1( 20),SCALE( 20),RV1( 20),RV2( 20),
     X        TCRIT( 8),EPSLON,RESDUL,DFL
      INTEGER  INT( 20),IERR( 8),MATCH( 4),LOW,UPP,ERROR,SAME,DIFF,
     X         ELEM,ORTH
      LOGICAL  SELECT( 20),SLHOLD( 20)
      DATA IREAD1/1/,IREADC/5/,IWRITE/6/
      DATA  SAME,DIFF,ELEM,ORTH/4HSAME,4HDIFF,1HE,1HO/
C
      OPEN(UNIT=IREAD1,FILE='FILE33')
      OPEN(UNIT=IREADC,FILE='FILE34')
      REWIND IREAD1
      REWIND IREADC
C
      NM = 20
      LCOUNT = 0
      WRITE(IWRITE,1)
    1 FORMAT(1H1,19X,57H EXPLANATION OF COLUMN ENTRIES FOR THE SUMMARY S
     XTATISTICS//1H ,95(1H-)/ 26X,9HBALANCING,15X,12HNO BALANCING,10X,
     X16HVECTORS COMPUTED/ 18X,3(24H------------------------,1X)/
     X16H ORDER LOW UPP T,2X,2(25H   HQR2   HQR     INVIT      )/
     X1H ,95(1H-)//95H 'BALANCING' IS THE OPTION THAT EMPLOYS SUBROUTINE
     X  BALANC  TO BALANCE THE MATRIX BEFORE THE   /
     X95H EIGENVALUES ARE COMPUTED AND  BALBAK  TO BACK TRANSFORM THE EI
     XGENVECTORS.                     //
     X95H 'VECTORS COMPUTED' IDENTIFIES BY POSITION, IN THE SET RETURNED
     X BY  HQR, THOSE EIGENVALUES     /
     X95H FOR WHICH  INVIT  COMPUTED THE ASSOCIATED EIGENVECTORS.  T  IN
     XDEXES AN EIGENVALUE FOR WHICH   /
     X66H THE EIGENVECTOR WAS COMPUTED AND  F  INDEXES A PASSED EIGENVAL
     XUE.    // 51H UNDER 'ORDER' IS THE ORDER OF EACH TEST MATRIX.   //
     X95H UNDER 'LOW' AND 'UPP' ARE INTEGERS INDICATING THE BOUNDARY IND
     XICES FOR THE BALANCED MATRIX.   //
     X95H UNDER 'T' IS THE LETTER E OR O INDICATING THE USE OF ELEMENTAR
     XY OR ORTHOGONAL TRANSFORMATIONS.//
     X95H UNDER 'HQR2   HQR' ARE TWO NUMBERS AND A KEYWORD.  THE FIRST N
     XUMBER, AN INTEGER, IS THE       )
      WRITE(IWRITE,2)
    2 FORMAT(
     X95H ABSOLUTE SUM OF THE ERROR FLAGS RETURNED SEPARATELY FROM  HQR2
     X  AND  HQR.  THE SECOND         /
     X95H NUMBER IS THE MEASURE OF PERFORMANCE BASED UPON THE RESIDUAL C
     XOMPUTED FOR THE  HQR2  PATH.    /
     X95H THE KEYWORD REPORTS THE DUPLICATION OF THE COMPUTED EIGENVALUE
     XS FROM  HQR2  AND  HQR.         /
     X95H 'SAME' MEANS THAT THE EIGENVALUES ARE EXACT DUPLICATES.  'DIFF
     X' MEANS THAT FOR AT LEAST ONE   /
     X95H PAIR OF CORRESPONDING EIGENVALUES, THE MEMBERS OF THE PAIR ARE
     X NOT IDENTICAL.                 //
     X62H UNDER 'INVIT' ARE TWO NUMBERS.  THE FIRST NUMBER, AN INTEGER,,
     X27H IS THE ABSOLUTE SUM OF THE      /
     X62H ERROR FLAGS RETURNED FROM THE PATH.  THE SECOND NUMBER IS THE,
     X29H MEASURE OF PERFORMANCE BASED    /
     X41H UPON THE RESIDUAL COMPUTED FOR THE PATH.//
     X62H -1.0  AS THE MEASURE OF PERFORMANCE IS PRINTED IF AN ERROR IN,
     X27H THE CORRESPONDING PATH HAS      /
     X47H PREVENTED THE COMPUTATION OF THE EIGENVECTORS./)
      WRITE(IWRITE,3)
    3 FORMAT(45H0FOR REDUCTIONS BY ELEMENTARY TRANSFORMATIONS/
     X95H THE  HQR2   PATH WITH    BALANCING USES THE EISPACK CODES  BAL
     XANC-ELMHES-ELTRAN-HQR2  -BALBAK,/
     X38H AS CALLED FROM DRIVER SUBROUTINE  RG. /
     X95H THE   HQR   PATH WITH    BALANCING USES THE EISPACK CODES  BAL
     XANC-ELMHES-HQR   ,              /
     X38H AS CALLED FROM DRIVER SUBROUTINE  RG. /
     X103H THE  INVIT  PATH WITH    BALANCING USES THE EISPACK CODES  BA
     XLANC-ELMHES-HQR   -INVIT -ELMBAK-BALBAK.  /
     X95H THE  HQR2   PATH WITH NO BALANCING USES THE EISPACK CODES  ELM
     XHES-ELTRAN-HQR2  .              /
     X95H THE   HQR   PATH WITH NO BALANCING USES THE EISPACK CODES  ELM
     XHES-HQR   .                     /
     X95H THE  INVIT  PATH WITH NO BALANCING USES THE EISPACK CODES  ELM
     XHES-HQR   -INVIT -ELMBAK.       )
      WRITE(IWRITE,4)
    4 FORMAT(45H0FOR REDUCTIONS BY ORTHOGONAL TRANSFORMATIONS/
     X95H THE  HQR2   PATH WITH    BALANCING USES THE EISPACK CODES  BAL
     XANC-ORTHES-ORTRAN-HQR2  -BALBAK./
     X95H THE   HQR   PATH WITH    BALANCING USES THE EISPACK CODES  BAL
     XANC-ORTHES-HQR   .              /
     X103H THE  INVIT  PATH WITH    BALANCING USES THE EISPACK CODES  BA
     XLANC-ORTHES-HQR   -INVIT -ORTBAK-BALBAK.  /
     X95H THE  HQR2   PATH WITH NO BALANCING USES THE EISPACK CODES  ORT
     XHES-ORTRAN-HQR2  .              /
     X95H THE   HQR   PATH WITH NO BALANCING USES THE EISPACK CODES  ORT
     XHES-HQR   .                     /
     X95H THE  INVIT  PATH WITH NO BALANCING USES THE EISPACK CODES  ORT
     XHES-HQR   -INVIT- ORTBAK.       )
      WRITE(IWRITE,15)
   15 FORMAT(1X,21HD.P. VERSION 04/15/83 )
    5 FORMAT( 53H1       TABULATION OF THE ERROR FLAG  ERROR  AND THE ,
     X    31HMEASURE OF PERFORMANCE  Y  FOR /5X,
     X    56HTHE  EISPACK  CODES.  THIS RUN DISPLAYS THESE STATISTICS ,
     X    30H FOR REAL GENERAL MATRICES.      //
     X    26X,9HBALANCING,15X,12HNO BALANCING,10X,16HVECTORS SELECTED/
     X    18X,3(24H------------------------,1X)/
     X    16H ORDER LOW UPP T,2X,2(25H   HQR2   HQR     INVIT      ))
   10 CALL RMATIN(NM,N,A,AHOLD,0)
      READ(IREADC,50) MM,(SELECT(I),I=1,N)
   50 FORMAT(I4/(72L1))
      DO  60  I = 1,N
   60 SLHOLD(I) = SELECT(I)
C
C     MM  AND  SELECT  ARE READ FROM SYSIN AFTER THE MATRIX IS
C     GENERATED.  MM  SPECIFIES TO  INVIT  THE MAXIMUM NUMBER OF
C     EIGENVECTORS THAT WILL BE COMPUTED.  SELECT  CONTAINS INFORMATION
C     ABOUT WHICH EIGENVECTORS ARE DESIRED.
C
      DO  300  ICALL = 1,12
         ICALL2 = MOD(ICALL,3)
         IF( ICALL2 .NE. 0 ) GO TO 80
         DO 70 I = 1,N
   70    SELECT(I) = SLHOLD(I)
   80    IF( ICALL .NE. 1 )  CALL  RMATIN(NM,N,A,AHOLD,1)
         GO TO  (90,100,110,120,130,140,160,165,170,175,180,185), ICALL
C
C     IF  HQR  PATH IS TAKEN THEN  HQR2  PATH MUST ALSO BE TAKEN
C     IN ORDER THAT COMPARISON OF EIGENVALUES BE POSSIBLE.
C
C
C     RGEWZ
C     INVOKED FROM DRIVER SUBROUTINE  RG.
C
   90    ICT = 1
         CALL  RG(NM,N,A,WR,WI,1,Z,INT,SCALE,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 ) GO TO 220
         GO TO  190
C
C     RGEW
C     INVOKED FROM DRIVER SUBROUTINE  RG.
C
  100    IF( IERR(1) .NE. 0 ) GO TO 300
         CALL  RG(NM,N,A,WR1,WI1,0,A,INT,SCALE,ERROR)
         IERR(1) = ERROR
         MATCH(1) = DIFF
         DO  101  I = 1,N
            IF( WR(I) .NE. WR1(I) .OR. WI(I) .NE. WI1(I) )  GO TO  300
  101    CONTINUE
         MATCH(1) = SAME
         GO TO  300
C
C     RGEW1Z
C
  110    ICT = 2
         CALL  BALANC(NM,N,A,LOW,UPP,SCALE)
         CALL  ELMHES(NM,N,LOW,UPP,A,INT)
         DO  112  I = 1,N
            DO  112  J = 1,N
  112         ASAVE(I,J) = A(I,J)
         CALL  HQR(NM,N,LOW,UPP,A,WR,WI,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 )  GO TO  220
         CALL  INVIT(NM,N,ASAVE,WR,WI,SELECT,MM,M,Z,ERROR,RM1,RV1,RV2)
         IERR(ICT) = IABS(ERROR)
         CALL  ELMBAK(NM,LOW,UPP,ASAVE,INT,M,Z)
         CALL  BALBAK(NM,N,LOW,UPP,SCALE,M,Z)
         GO TO  190
C
C     RGNEWZ
C
  120    ICT = 3
         CALL  ELMHES(NM,N,1,N,A,INT)
         CALL  ELTRAN(NM,N,1,N,A,INT,Z)
         CALL  HQR2(NM,N,1,N,A,WR,WI,Z,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 ) GO TO 220
         DO  121  I = 1,N
            WR1(I) = WR(I)
  121       WI1(I) = WI(I)
         GO TO  190
C
C     RGNEW
C
  130    IF( IERR(3) .NE. 0 ) GO TO 300
         CALL  ELMHES(NM,N,1,N,A,INT)
         CALL  HQR(NM,N,1,N,A,WR,WI,ERROR)
         IERR(3) = ERROR
         MATCH(2) = DIFF
         DO  131  I = 1,N
            IF( WR(I) .NE. WR1(I) .OR. WI(I) .NE. WI1(I) )  GO TO  300
  131    CONTINUE
         MATCH(2) = SAME
         GO TO  300
C
C     RGNEW1Z
C
  140    ICT = 4
         CALL  ELMHES(NM,N,1,N,A,INT)
         DO  142  I = 1,N
            DO  142  J = 1,N
  142         ASAVE(I,J) = A(I,J)
         CALL  HQR(NM,N,1,N,A,WR,WI,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 )  GO TO  220
         CALL  INVIT(NM,N,ASAVE,WR,WI,SELECT,MM,M,Z,ERROR,RM1,RV1,RV2)
         IERR(ICT) = IABS(ERROR)
         CALL  ELMBAK(NM,1,N,ASAVE,INT,M,Z)
         GO TO 190
C
C     RGOWZ
C
  160    ICT = 5
         CALL  BALANC(NM,N,A,LOW,UPP,SCALE)
         CALL  ORTHES(NM,N,LOW,UPP,A,ORT)
         CALL  ORTRAN(NM,N,LOW,UPP,A,ORT,Z)
         CALL  HQR2(NM,N,LOW,UPP,A,WR,WI,Z,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 ) GO TO 220
         DO  161  I = 1,N
            WR1(I) = WR(I)
  161       WI1(I) = WI(I)
         CALL  BALBAK(NM,N,LOW,UPP,SCALE,N,Z)
         GO TO  190
C
C     RGOW
C
  165    IF( IERR(5) .NE. 0 ) GO TO 300
         CALL  BALANC(NM,N,A,LOW,UPP,SCALE)
         CALL  ORTHES(NM,N,LOW,UPP,A,ORT)
         CALL  HQR(NM,N,LOW,UPP,A,WR,WI,ERROR)
         IERR(5) = ERROR
         MATCH(3) = DIFF
         DO  166  I = 1,N
            IF( WR(I) .NE. WR1(I) .OR. WI(I) .NE. WI1(I) )  GO TO  300
  166    CONTINUE
         MATCH(3) = SAME
         GO TO  300
C
C     RGOW1Z
C
  170    ICT = 6
         CALL  BALANC(NM,N,A,LOW,UPP,SCALE)
         CALL  ORTHES(NM,N,LOW,UPP,A,ORT)
         DO  172  I = 1,N
            DO  172  J = 1,N
  172         ASAVE(I,J) = A(I,J)
         CALL  HQR(NM,N,LOW,UPP,A,WR,WI,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 )  GO TO  220
         CALL  INVIT(NM,N,ASAVE,WR,WI,SELECT,MM,M,Z,ERROR,RM1,RV1,RV2)
         IERR(ICT) = IABS(ERROR)
         CALL  ORTBAK(NM,LOW,UPP,ASAVE,ORT,M,Z)
         CALL  BALBAK(NM,N,LOW,UPP,SCALE,M,Z)
         GO TO  190
C
C     RGNOWZ
C
  175    ICT = 7
         CALL  ORTHES(NM,N,1,N,A,ORT)
         CALL  ORTRAN(NM,N,1,N,A,ORT,Z)
         CALL  HQR2(NM,N,1,N,A,WR,WI,Z,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 ) GO TO 220
         DO  176  I = 1,N
            WR1(I) = WR(I)
  176       WI1(I) = WI(I)
         GO TO  190
C
C     RGNOW
C
  180    IF( IERR(7) .NE. 0 ) GO TO 300
         CALL  ORTHES(NM,N,1,N,A,ORT)
         CALL  HQR(NM,N,1,N,A,WR,WI,ERROR)
         IERR(7) = ERROR
         MATCH(4) = DIFF
         DO  181  I = 1,N
            IF( WR(I) .NE. WR1(I) .OR. WI(I) .NE. WI1(I) )  GO TO  300
  181    CONTINUE
         MATCH(4) = SAME
         GO TO  300
C
C     RGNOW1Z
C
  185    ICT = 8
         CALL  ORTHES(NM,N,1,N,A,ORT)
         DO  187  I = 1,N
            DO  187  J = 1,N
  187         ASAVE(I,J) = A(I,J)
         CALL  HQR(NM,N,1,N,A,WR,WI,ERROR)
         IERR(ICT) = ERROR
         IF( ERROR .NE. 0 )  GO TO  220
         CALL  INVIT(NM,N,ASAVE,WR,WI,SELECT,MM,M,Z,ERROR,RM1,RV1,RV2)
         IERR(ICT) = IABS(ERROR)
         CALL  ORTBAK(NM,1,N,ASAVE,ORT,M,Z)
C
  190    CALL  RMATIN(NM,N,A,AHOLD,1)
         IF( ICALL2 .EQ. 0 ) CALL RGW1ZR(NM,N,A,WR,WI,SELECT,
     X                                   M,Z,RV1,RESDUL)
         IF( ICALL2 .EQ. 1 ) CALL RGWZR(NM,N,A,WR,WI,Z,RV1,RESDUL)
         DFL = 10 * N
         TCRIT(ICT) = RESDUL/EPSLON(DFL)
         GO TO 300
  220    TCRIT(ICT) = -1.0D0
  300 CONTINUE
C
      IF( MOD(LCOUNT,16) .EQ. 0 ) WRITE(IWRITE,5)
      LCOUNT = LCOUNT + 1
      WRITE(IWRITE,520)
     X              N,LOW,UPP,ELEM,(IERR(2*I-1),TCRIT(2*I-1),MATCH(I),
     X              IERR(2*I),TCRIT(2*I),I=1,2),(SELECT(I),I=1,N)
  520 FORMAT(I4,1X,2I4,2X,A1,2(I4,F6.3,1X,A4,I4,F6.3),
     X       4X,21L1/(72X,20L1))
      WRITE(IWRITE,530) ORTH,(IERR(2*I-1),TCRIT(2*I-1),MATCH(I),
     X              IERR(2*I),TCRIT(2*I),I=3,4)
  530 FORMAT(15X,A1,2(I4,F6.3,1X,A4,I4,F6.3))
      GO TO  10
      END
      SUBROUTINE RGWZR(NM,N,A,WR,WI,Z,NORM,RESDUL)
C
      DOUBLE PRECISION NORM(N),WR(N),WI(N),A(NM,N),Z(NM,N),
     X       NORMA,TNORM,XR,XI,S,SUM,SUMA,SUMZ,SUMR,SUMI,RESDUL,PYTHAG
      LOGICAL CPLX
C
C     THIS SUBROUTINE FORMS THE 1-NORM OF THE RESIDUAL MATRIX
C     A*Z-Z*DIAG(W) WHERE A IS A REAL GENERAL MATRIX, Z IS
C     A MATRIX WHICH CONTAINS THE EIGENVECTORS OF A, AND W
C     IS A VECTOR WHICH CONTAINS THE EIGENVALUES OF A. ALL
C     NORMS APPEARING IN THE COMMENTS BELOW ARE 1-NORMS.
C
C     THIS SUBROUTINE IS CATALOGUED AS EISPDRV4(RGWZR).
C
C     INPUT.
C
C        NM IS THE ROW DIMENSION OF TWO-DIMENSIONAL ARRAY PARAMETERS
C           AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT;
C
C        N IS THE ORDER OF THE MATRIX  A;
C
C        A(NM,N) IS A REAL GENERAL MATRIX;
C
C        Z(NM,N) IS A MATRIX WHICH CONTAINS THE REAL AND IMAGINARY PARTS
C           OF THE EIGENVECTORS OF A.  THE I-TH COLUMN OF Z IS A REAL
C           EIGENVECTOR IF THE I-TH EIGENVALUE IS REAL.  IF THE I-TH
C           EIGENVALUE IS COMPLEX AND THE FIRST OF A CONJUGATE PAIR, THE
C           I-TH AND (I+1)-TH COLUMNS OF  Z  CONTAIN THE REAL AND
C           IMAGINARY PARTS OF ITS EIGENVECTORS;
C
C        WR(N), WI(N) ARE ARRAYS CONTAINING THE REAL AND IMAGINARY
C           PARTS OF THE EIGENVALUES OF  A.
C
C     OUTPUT.
C
C        Z(NM,N) IS AN ARRAY WHICH CONTAINS THE NORMALIZED
C           APPROXIMATE EIGENVECTORS OF A.  THE EIGENVECTORS ARE
C           NORMALIZED USING THE 1-NORM IN SUCH A WAY THAT THE FIRST
C           ELEMENT WHOSE MAGNITUDE IS LARGER THAN THE NORM OF THE
C           EIGENVECTOR DIVIDED BY N IS REAL AND POSITIVE;
C
C        NORM(N) IS AN ARRAY SUCH THAT FOR EACH K,
C           NORM(K) = !!A*Z(K)-Z(K)*W(K)!!/(!!A!!*!!Z(K)!!)
C           WHERE Z(K) IS THE K-TH EIGENVECTOR;
C
C        RESDUL IS THE REAL NUMBER
C           !!A*Z-Z*DIAG(W)!!/(!!A!!*!!Z!!).
C
C     ----------------------------------------------------------------
C
      NORMA = 0.0D0
      RESDUL = 0.0D0
      CPLX = .FALSE.
C
      DO 20 I=1,N
         SUMA = 0.0D0
C
         DO 10 L=1,N
   10       SUMA = SUMA + DABS(A(L,I))
C
   20    NORMA = DMAX1(NORMA,SUMA)
C
      IF(NORMA .EQ. 0.0D0) NORMA = 1.0D0
C
      DO 160 I=1,N
         IF(CPLX) GO TO 80
         S = 0.0D0
         SUMZ = 0.0D0
         IF(WI(I) .NE. 0.0D0) GO TO 90
C        ..........THIS IS FOR REAL EIGENVALUES..........
         DO 40 L=1,N
            SUMZ = SUMZ + DABS(Z(L,I))
            SUM = -WR(I)*Z(L,I)
C
            DO 30 K=1,N
   30          SUM = SUM + A(L,K)*Z(K,I)
C
   40       S = S + DABS(SUM)
C
         NORM(I) = SUMZ
C        ..........THIS LOOP WILL NEVER BE COMPLETED SINCE THERE
C                  WILL ALWAYS EXIST AN ELEMENT IN THE VECTOR Z(I)
C                  LARGER THAN !!Z(I)!!/N..........
         DO 50 L=1,N
            IF(DABS(Z(L,I)) .GE. NORM(I)/N) GO TO 60
   50       CONTINUE
C
   60    TNORM = DSIGN(NORM(I),Z(L,I))
C
         DO 70 L=1,N
   70       Z(L,I) = Z(L,I)/TNORM
C
         NORM(I) = S/(NORM(I)*NORMA)
   80    CPLX = .FALSE.
         GO TO 150
C        ..........THIS IS FOR COMPLEX EIGENVALUES..........
   90    DO 110 L = 1,N
            SUMZ = SUMZ + PYTHAG(Z(L,I),Z(L,I+1))
            SUMR = -WR(I)*Z(L,I) + WI(I)*Z(L,I+1)
            SUMI = -WR(I)*Z(L,I+1) - WI(I)*Z(L,I)
C
            DO 100 K=1,N
               SUMR = SUMR + A(L,K)*Z(K,I)
  100          SUMI = SUMI + A(L,K)*Z(K,I+1)
C
  110       S = S + PYTHAG(SUMR,SUMI)
C
         NORM(I) = SUMZ
C        ..........THIS LOOP WILL NEVER BE COMPLETED SINCE THERE WILL
C                  ALWAYS EXIST AN ELEMENT IN THE VECTOR Z(I) LARGER
C                  THAN !!Z(I)!!/N..........
         DO 120 L=1,N
            IF(PYTHAG(Z(L,I),Z(L,I+1)) .GE. NORM(I)/N)
     1      GO TO 130
  120       CONTINUE
  130    CONTINUE
C
         XR = PYTHAG(Z(L,I),Z(L,I+1))            
         XR = NORM(I)*Z(L,I)/PYTHAG(Z(L,I),Z(L,I+1))
         XI = NORM(I)*Z(L,I+1)/PYTHAG(Z(L,I),Z(L,I+1))
C
         DO 140 L= 1,N
            CALL CDIV(Z(L,I),Z(L,I+1),XR,XI,Z(L,I),Z(L,I+1))
  140    CONTINUE
C
         NORM(I) = S/(NORM(I)*NORMA)
         NORM(I+1) = NORM(I)
         CPLX = .TRUE.
  150    RESDUL = DMAX1(NORM(I),RESDUL)
  160    CONTINUE
C
      RETURN
      END
      SUBROUTINE RGW1ZR(NM,N,A,WR,WI,SELECT,M,Z,NORM,RESDUL)
C
      DOUBLE PRECISION NORM(N),WR(N),WI(N),A(NM,N),Z(NM,M),
     X       NORMA,TNORM,XR,XI,S,SUM,SUMA,SUMZ,SUMR,SUMI,RESDUL,PYTHAG
      LOGICAL SELECT(N), CPLX
C
C     THIS SUBROUTINE FORMS THE 1-NORM OF THE RESIDUAL MATRIX
C     A*Z-Z*DIAG(W)   WHERE  A  IS A REAL GENERAL MATRIX,  Z  IS
C     A MATRIX WHICH CONTAINS SOME OF THE EIGENVECTORS OF  A,
C     AND  W  IS A VECTOR WHICH CONTAINS THE EIGENVALUES OF  A.
C     ALL NORMS APPEARING IN THE COMMENTS BELOW ARE 1-NORMS.
C
C     THIS SUBROUTINE IS CATALOGUED AS EISPDRV4(RGW1ZR).
C
C     INPUT.
C
C        NM  IS THE ROW DIMENSION OF TWO-DIMENSIONAL ARRAY PARAMETERS
C           AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT.
C
C        N  IS THE ORDER OF THE MATRIX  A.
C
C        A(NM,N)  IS A REAL GENERAL MATRIX.
C
C        Z(NM,M)  IS A MATRIX WHICH CONTAINS THE REAL AND IMAGINARY
C           OF THE EIGENVECTORS OF  A.  THE EIGENVECTORS ARE STORED
C           PARTS IN PACKED FORM, THAT IS, USING ONE COLUMN FOR EACH
C           REAL EIGENVECTOR AND USING TWO COLUMNS FOR EACH COMPLEX
C           EIGENVECTOR, THE EIGENVECTORS CORRESPONDING TO THE FLAGGED
C           EIGENVALUES ARE STORED IN CONSECUTIVE COLUMNS, STARTING IN
C           COLUMN  1.  (NOTE. FOR THE DEFINITION OF FLAGGED EIGENVALUES
C           SEE THE DESCRIPTION OF THE SELECT ARRAY.)
C
C        WR(N),WI(N)  ARE ARRAYS CONTAINING THE REAL AND IMAGINARY
C           PARTS OF THE EIGENVALUES OF  A.
C
C        SELECT(N)  IS A LOGICAL ARRAY WHICH SPECIFIES THE
C           FLAGGED EIGENVALUES WHOSE EIGENVECTORS ARE
C           CONTAINED IN THE MATRIX  Z.  THE I-TH EIGENVALUE
C           (WR(I),WI(I))  IS FLAGGED IF  SELECT(I) = .TRUE.
C
C        M  IS THE NUMBER OF COLUMNS OF  Z  CONTAINING EIGENVECTORS
C           OF  A.
C
C     OUTPUT.
C
C        Z(NM,M)  IS AN ARRAY WHICH CONTAINS THE NORMALIZED
C           APPROXIMATE EIGENVECTORS OF  A  IN PACKED FORM AS
C           DESCRIBED ABOVE.  THE EIGENVECTORS ARE NORMALIZED
C           USING THE 1-NORM IN SUCH A WAY THAT THE FIRST ELEMENT
C           WHOSE MAGNITUDE IS LARGER THAN THE NORM OF THE
C           EIGENVECTOR DIVIDED BY  N  IS REAL AND POSITIVE.
C
C        NORM(N)  IS AN ARRAY SUCH THAT WHENEVER SELECT(K)  = .TRUE.
C           NORM(K) = !!A*Z(K)-Z(K)*W(K)!!/(!!A!!*!!Z(K)!!)
C           WHERE  Z(K)  IS THE EIGENVECTOR CORRESPONDING TO THE
C           K-TH EIGENVALUE  (WR(K),WI(K)).
C
C        RESDUL IS THE REAL NUMBER
C           !!A*Z-Z*DIAG(W)!!/(!!A!!*!!Z!!)
C
C     ----------------------------------------------------------------
C
      KCOM = 0
      NORMA = 0.0D0
      RESDUL = 0.0D0
      IF( M .EQ. 0 ) RETURN
      CPLX = .FALSE.
      II=1
C
      DO 20 I=1,N
         SUMA = 0.0D0
C
         DO 10 L=1,N
   10       SUMA = SUMA + DABS(A(L,I))
C
   20    NORMA = DMAX1(NORMA,SUMA)
C
      IF(NORMA .EQ. 0.0D0) NORMA = 1.0D0
C
      DO 160 I=1,N
         IF(CPLX) GO TO 80
         S = 0.0D0
         SUMZ = 0.0D0
         IF(WI(I) .NE. 0.0D0) GO TO 90
         IF( .NOT. SELECT(I)) GO TO 160
C        ..........THIS IS FOR REAL EIGENVALUES..........
         DO 40 L=1,N
            SUMZ = SUMZ + DABS(Z(L,II))
            SUM = -WR(I)*Z(L,II)
C
            DO 30 K=1,N
   30          SUM = SUM + A(L,K)*Z(K,II)
C
   40       S = S + DABS(SUM)
C
         IF(SUMZ .NE. 0.0D0) GO TO 45
         NORM(I) = -1.0D0
         GO TO 150
   45    NORM(I) = SUMZ
C        ..........THIS LOOP WILL NEVER BE COMPLETED SINCE THERE
C                  WILL ALWAYS EXIST AN ELEMENT IN THE VECTOR Z(II)
C                  LARGER THAN !!Z(II)!!/N..........
         DO 50 L=1,N
            IF(DABS(Z(L,II)) .GE. NORM(I)/N) GO TO 60
   50       CONTINUE
C
   60    TNORM = DSIGN(NORM(I),Z(L,II))
C
         DO 70 J=1,N
   70       Z(J,II) = Z(J,II)/TNORM
C
         NORM(I) = S/(NORM(I)*NORMA)
         GO TO 150
   80    CPLX = .FALSE.
         GO TO 155
C        ..........THIS IS FOR COMPLEX EIGENVALUES..........
   90    IF(SELECT(I)) GO TO 95
         KCOM = MOD(KCOM+1,2)
         GO TO 160
C
   95    DO 110 L = 1,N
            SUMZ = SUMZ + PYTHAG(Z(L,II),Z(L,II+1))
            SUMR = -WR(I)*Z(L,II) + WI(I)*Z(L,II+1)
            SUMI = -WR(I)*Z(L,II+1) - WI(I)*Z(L,II)
C
            DO 100 K=1,N
               SUMR = SUMR + A(L,K)*Z(K,II)
  100          SUMI = SUMI + A(L,K)*Z(K,II+1)
C
  110       S = S + PYTHAG(SUMR,SUMI)
C
         IF(SUMZ .NE. 0.0D0) GO TO 115
         NORM(I) = -1.0D0
         GO TO 145
  115    NORM(I) = SUMZ
C        ..........THIS LOOP WILL NEVER BE COMPLETED SINCE THERE WILL
C                  ALWAYS EXIST AN ELEMENT IN THE VECTOR Z(II) LARGER
C                  THAN !!Z(II)!!/N..........
         DO 120 L=1,N
            IF(PYTHAG(Z(L,II),Z(L,II+1)) .GE. NORM(I)/N)
     1      GO TO 130
  120       CONTINUE
C
  130    XR = NORM(I)*Z(L,II)/PYTHAG(Z(L,II),Z(L,II+1))
         XI = NORM(I)*Z(L,II+1)/PYTHAG(Z(L,II),Z(L,II+1))
C
         DO 140 J= 1,N
            CALL CDIV(Z(J,II),Z(J,II+1),XR,XI,Z(J,II),Z(J,II+1))
  140    CONTINUE
C
         NORM(I) = S/(NORM(I)*NORMA)
  145    CPLX = .TRUE.
         IF(KCOM .EQ. 0) GO TO 150
         CPLX = .FALSE.
         II = II+1
  150    RESDUL = DMAX1(NORM(I),RESDUL)
  155    II = II+1
  160    CONTINUE
C
      RETURN
      END
      SUBROUTINE RMATIN(NM,N,A,AHOLD,INITIL)
C
C     THIS INPUT SUBROUTINE READS A REAL MATRIX FROM SYSIN OF
C     ORDER N.
C     TO GENERATE THE MATRIX  A  INITIALLY,  INITIL  IS TO BE 0.
C     TO REGENERATE THE MATRIX  A  FOR THE PURPOSE OF THE RESIDUAL
C     CALCULATION,  INITIL  IS TO BE  1.
C
C     THIS ROUTINE IS CATALOGUED AS  EISPDRV4(RGREADI).
C
      DOUBLE PRECISION A(NM,NM),AHOLD(NM,NM)
      INTEGER  IA( 20)
      DATA IREADA/1/,IWRITE/6/
C
      IF( INITIL .EQ. 1 )  GO TO  30
      READ(IREADA,5)N, M
    5 FORMAT(I6,6X,I6)
      IF( N .EQ. 0 )  GO TO  70
      IF (M .NE. 1) GO TO 14
      DO  13  I = 1,N
         READ(IREADA,16) (IA(J), J=I,N)
         DO  12  J = I,N
           A(I,J) = IA(J)
   12      A(J,I) = A(I,J)
   13 CONTINUE
      GO TO 19
   14 DO  17  I = 1,N
         READ(IREADA,16) (IA(J), J=1,N)
   16    FORMAT(6I12)
         DO  17  J = 1,N
   17      A(I,J) = IA(J)
   19 DO  20  I = 1,N
         DO  20  J = 1,N
   20      AHOLD(I,J) = A(I,J)
      RETURN
   30 DO  40  I = 1,N
         DO  40  J = 1,N
   40      A(I,J) = AHOLD(I,J)
      RETURN
   70 WRITE(IWRITE,80)
   80 FORMAT(46H0END OF DATA FOR SUBROUTINE  RMATIN(RGREADI). /1H1)
      STOP
      END