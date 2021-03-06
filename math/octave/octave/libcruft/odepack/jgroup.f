      SUBROUTINE JGROUP (N,IA,JA,MAXG,NGRP,IGP,JGP,INCL,JDONE,IER)
CLLL. OPTIMIZE
      INTEGER N, IA, JA, MAXG, NGRP, IGP, JGP, INCL, JDONE, IER
      DIMENSION IA(1), JA(1), IGP(1), JGP(N), INCL(N), JDONE(N)
C-----------------------------------------------------------------------
C THIS SUBROUTINE CONSTRUCTS GROUPINGS OF THE COLUMN INDICES OF
C THE JACOBIAN MATRIX, USED IN THE NUMERICAL EVALUATION OF THE
C JACOBIAN BY FINITE DIFFERENCES.
C
C INPUT.. 
C N      = THE ORDER OF THE MATRIX.
C IA,JA  = SPARSE STRUCTURE DESCRIPTORS OF THE MATRIX BY ROWS.
C MAXG   = LENGTH OF AVAILABLE STORATE IN THE IGP ARRAY.
C
C OUTPUT..
C NGRP   = NUMBER OF GROUPS.
C JGP    = ARRAY OF LENGTH N CONTAINING THE COLUMN INDICES BY GROUPS. 
C IGP    = POINTER ARRAY OF LENGTH NGRP + 1 TO THE LOCATIONS IN JGP
C          OF THE BEGINNING OF EACH GROUP.
C IER    = ERROR INDICATOR.  IER = 0 IF NO ERROR OCCURRED, OR 1 IF
C          MAXG WAS INSUFFICIENT.
C
C INCL AND JDONE ARE WORKING ARRAYS OF LENGTH N.
C-----------------------------------------------------------------------
      INTEGER I, J, K, KMIN, KMAX, NCOL, NG
C
      IER = 0
      DO 10 J = 1,N 
 10     JDONE(J) = 0
      NCOL = 1
      DO 60 NG = 1,MAXG
        IGP(NG) = NCOL
        DO 20 I = 1,N
 20       INCL(I) = 0
        DO 50 J = 1,N
C REJECT COLUMN J IF IT IS ALREADY IN A GROUP.--------------------------
          IF (JDONE(J) .EQ. 1) GO TO 50 
          KMIN = IA(J)
          KMAX = IA(J+1) - 1
          DO 30 K = KMIN,KMAX 
C REJECT COLUMN J IF IT OVERLAPS ANY COLUMN ALREADY IN THIS GROUP.------
            I = JA(K)
            IF (INCL(I) .EQ. 1) GO TO 50
 30         CONTINUE
C ACCEPT COLUMN J INTO GROUP NG.----------------------------------------
          JGP(NCOL) = J
          NCOL = NCOL + 1
          JDONE(J) = 1
          DO 40 K = KMIN,KMAX 
            I = JA(K)
 40         INCL(I) = 1
 50       CONTINUE
C STOP IF THIS GROUP IS EMPTY (GROUPING IS COMPLETE).-------------------
        IF (NCOL .EQ. IGP(NG)) GO TO 70 
 60     CONTINUE
C ERROR RETURN IF NOT ALL COLUMNS WERE CHOSEN (MAXG TOO SMALL).---------
      IF (NCOL .LE. N) GO TO 80
      NG = MAXG
 70   NGRP = NG - 1 
      RETURN
 80   IER = 1
      RETURN
C----------------------- END OF SUBROUTINE JGROUP ----------------------
      END 
