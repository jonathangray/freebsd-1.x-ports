      SUBROUTINE AINVG (RES, ADDA, NEQ, T, Y, YDOT, MITER,
     1                   ML, MU, PW, IPVT, IER )
CLLL. OPTIMIZE
      EXTERNAL RES, ADDA
      INTEGER NEQ, MITER, ML, MU, IPVT, IER
      INTEGER I, LENPW, MLP1, NROWPW
      DOUBLE PRECISION T, Y, YDOT, PW
      DIMENSION Y(1), YDOT(1), PW(1), IPVT(1)
C-----------------------------------------------------------------------
C THIS SUBROUTINE COMPUTES THE INITIAL VALUE
C OF THE VECTOR YDOT SATISFYING
C     A * YDOT = G(T,Y)
C WHEN A IS NONSINGULAR.  IT IS CALLED BY LSODI FOR
C INITIALIZATION ONLY, WHEN ISTATE = 0 .
C AINVG RETURNS AN ERROR FLAG IER..
C   IER  =  0  MEANS AINVG WAS SUCCESSFUL.
C   IER .GE. 2 MEANS RES RETURNED AN ERROR FLAG IRES = IER. 
C   IER .LT. 0 MEANS THE A-MATRIX WAS FOUND TO BE SINGULAR. 
C-----------------------------------------------------------------------
C
      IF (MITER .GE. 4)  GO TO 100
C
C FULL MATRIX CASE -----------------------------------------------------
C
      LENPW = NEQ*NEQ
      DO 10  I = 1, LENPW
   10    PW(I) = 0.0D0
C
      IER = 1
      CALL RES ( NEQ, T, Y, PW, YDOT, IER )
      IF (IER .GT. 1) RETURN
C
      CALL ADDA ( NEQ, T, Y, 0, 0, PW, NEQ )
      CALL DGEFA ( PW, NEQ, NEQ, IPVT, IER )
      IF (IER .EQ. 0) GO TO 20
         IER = -IER 
         RETURN
   20 CALL DGESL ( PW, NEQ, NEQ, IPVT, YDOT, 0 )
      RETURN
C
C BAND MATRIX CASE -----------------------------------------------------
C
  100 CONTINUE
      NROWPW = 2*ML + MU + 1
      LENPW = NEQ * NROWPW
      DO 110  I = 1, LENPW
  110    PW(I) = 0.0D0
C
      IER = 1
      CALL RES ( NEQ, T, Y, PW, YDOT, IER )
      IF (IER .GT. 1) RETURN
C
      MLP1 = ML + 1 
      CALL ADDA ( NEQ, T, Y, ML, MU, PW(MLP1), NROWPW )
      CALL DGBFA ( PW, NROWPW, NEQ, ML, MU, IPVT, IER )
      IF (IER .EQ. 0) GO TO 120
         IER = -IER 
         RETURN
  120 CALL DGBSL ( PW, NROWPW, NEQ, ML, MU, IPVT, YDOT, 0 ) 
      RETURN
C-------------------- END OF SUBROUTINE AINVG --------------------------
      END 
