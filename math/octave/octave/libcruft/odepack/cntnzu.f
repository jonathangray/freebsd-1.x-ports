      SUBROUTINE CNTNZU (N, IA, JA, NZSUT)
      INTEGER N, IA, JA, NZSUT
      DIMENSION IA(1), JA(1)
C-----------------------------------------------------------------------
C THIS ROUTINE COUNTS THE NUMBER OF NONZERO ELEMENTS IN THE STRICT
C UPPER TRIANGLE OF THE MATRIX M + M(TRANSPOSE), WHERE THE SPARSITY
C STRUCTURE OF M IS GIVEN BY POINTER ARRAYS IA AND JA.
C THIS IS NEEDED TO COMPUTE THE STORAGE REQUIREMENTS FOR THE
C SPARSE MATRIX REORDERING OPERATION IN ODRV.
C-----------------------------------------------------------------------
      INTEGER II, JJ, J, JMIN, JMAX, K, KMIN, KMAX, NUM
C
      NUM = 0
      DO 50 II = 1,N
        JMIN = IA(II)
        JMAX = IA(II+1) - 1
        IF (JMIN .GT. JMAX) GO TO 50
        DO 40 J = JMIN,JMAX
          IF (JA(J) - II) 10, 40, 30
 10       JJ =JA(J) 
          KMIN = IA(JJ)
          KMAX = IA(JJ+1) - 1 
          IF (KMIN .GT. KMAX) GO TO 30
          DO 20 K = KMIN,KMAX 
            IF (JA(K) .EQ. II) GO TO 40 
 20         CONTINUE
 30       NUM = NUM + 1
 40       CONTINUE
 50     CONTINUE
      NZSUT = NUM
      RETURN
C----------------------- END OF SUBROUTINE CNTNZU ----------------------
      END 
