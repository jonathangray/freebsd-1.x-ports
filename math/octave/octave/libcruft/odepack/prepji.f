      SUBROUTINE PREPJI (NEQ, Y, YH, NYH, EWT, RTEM, SAVR, S, WM, IWM,
     1   RES, JAC, ADDA)
CLLL. OPTIMIZE
      EXTERNAL RES, JAC, ADDA 
      INTEGER NEQ, NYH, IWM
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L, METH, MITER,
     2   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NRE, NJE, NQU
      INTEGER I, I1, I2, IER, II, IRES, J, J1, JJ, LENP,
     1   MBA, MBAND, MEB1, MEBAND, ML, ML3, MU
      DOUBLE PRECISION Y, YH, EWT, RTEM, SAVR, S, WM
      DOUBLE PRECISION ROWNS, 
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION CON, FAC, HL0, R, SRUR, YI, YJ, YJJ
      DIMENSION NEQ(1), Y(1), YH(NYH,1), EWT(1), RTEM(1),
     1   S(1), SAVR(1), WM(1), IWM(1)
      COMMON /LS0001/ ROWNS(209),
     2   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     3   IOWND(14), IOWNS(6), 
     4   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NRE, NJE, NQU
C-----------------------------------------------------------------------
C PREPJI IS CALLED BY STODI TO COMPUTE AND PROCESS THE MATRIX
C P = A - H*EL(1)*J , WHERE J IS AN APPROXIMATION TO THE JACOBIAN DR/DY,
C WHERE R = G(T,Y) - A(T,Y)*S. HERE J IS COMPUTED BY THE USER-SUPPLIED
C ROUTINE JAC IF MITER = 1 OR 4, OR BY FINITE DIFFERENCING IF MITER = 
C 2 OR 5. J IS STORED IN WM, RESCALED, AND ADDA IS CALLED TO GENERATE 
C P. P IS THEN SUBJECTED TO LU DECOMPOSITION IN PREPARATION 
C FOR LATER SOLUTION OF LINEAR SYSTEMS WITH P AS COEFFICIENT
C MATRIX.  THIS IS DONE BY DGEFA IF MITER = 1 OR 2, AND BY
C DGBFA IF MITER = 4 OR 5.
C
C IN ADDITION TO VARIABLES DESCRIBED PREVIOUSLY, COMMUNICATION
C WITH PREPJI USES THE FOLLOWING..
C Y     = ARRAY CONTAINING PREDICTED VALUES ON ENTRY.
C RTEM  = WORK ARRAY OF LENGTH N (ACOR IN STODI). 
C SAVR  = ARRAY USED FOR OUTPUT ONLY.  ON OUTPUT IT CONTAINS THE
C         RESIDUAL EVALUATED AT CURRENT VALUES OF T AND Y.
C S     = ARRAY CONTAINING PREDICTED VALUES OF DY/DT (SAVF IN STODI). 
C WM    = REAL WORK SPACE FOR MATRICES.  ON OUTPUT IT CONTAINS THE
C         LU DECOMPOSITION OF P.
C         STORAGE OF MATRIX ELEMENTS STARTS AT WM(3).
C         WM ALSO CONTAINS THE FOLLOWING MATRIX-RELATED DATA..
C         WM(1) = SQRT(UROUND), USED IN NUMERICAL JACOBIAN INCREMENTS.
C IWM   = INTEGER WORK SPACE CONTAINING PIVOT INFORMATION, STARTING AT
C         IWM(21).  IWM ALSO CONTAINS THE BAND PARAMETERS
C         ML = IWM(1) AND MU = IWM(2) IF MITER IS 4 OR 5.
C EL0   = EL(1) (INPUT).
C IERPJ = OUTPUT ERROR FLAG.
C         = 0 IF NO TROUBLE OCCURRED,
C         = 1 IF THE P MATRIX WAS FOUND TO BE SINGULAR,
C         = IRES (= 2 OR 3) IF RES RETURNED IRES = 2 OR 3.
C JCUR  = OUTPUT FLAG = 1 TO INDICATE THAT THE JACOBIAN MATRIX
C         (OR APPROXIMATION) IS NOW CURRENT.
C THIS ROUTINE ALSO USES THE COMMON VARIABLES EL0, H, TN, UROUND,
C MITER, N, NRE, AND NJE.
C-----------------------------------------------------------------------
      NJE = NJE + 1 
      HL0 = H*EL0
      IERPJ = 0
      JCUR = 1
      GO TO (100, 200, 300, 400, 500), MITER
C IF MITER = 1, CALL RES, THEN JAC, AND MULTIPLY BY SCALAR. ------------
 100  IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NRE = NRE + 1 
      IF (IRES .GT. 1) GO TO 600
      LENP = N*N
      DO 110 I = 1,LENP
 110    WM(I+2) = 0.0D0
      CALL JAC ( NEQ, TN, Y, S, 0, 0, WM(3), N )
      CON = -HL0
      DO 120 I = 1,LENP
 120    WM(I+2) = WM(I+2)*CON 
      GO TO 240
C IF MITER = 2, MAKE N + 1 CALLS TO RES TO APPROXIMATE J. --------------
 200  CONTINUE
      IRES = -1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NRE = NRE + 1 
      IF (IRES .GT. 1) GO TO 600
      SRUR = WM(1)
      J1 = 2
      DO 230 J = 1,N
        YJ = Y(J)
        R = DMAX1(SRUR*DABS(YJ),0.01D0/EWT(J))
        Y(J) = Y(J) + R
        FAC = -HL0/R
        CALL RES ( NEQ, TN, Y, S, RTEM, IRES )
        NRE = NRE + 1
        IF (IRES .GT. 1) GO TO 600
        DO 220 I = 1,N
 220      WM(I+J1) = (RTEM(I) - SAVR(I))*FAC
        Y(J) = YJ
        J1 = J1 + N 
 230    CONTINUE
      IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NRE = NRE + 1 
      IF (IRES .GT. 1) GO TO 600
C ADD MATRIX A. --------------------------------------------------------
 240  CONTINUE
      CALL ADDA(NEQ, TN, Y, 0, 0, WM(3), N)
C DO LU DECOMPOSITION ON P. --------------------------------------------
      CALL DGEFA (WM(3), N, N, IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C DUMMY SECTION FOR MITER = 3 
 300  RETURN
C IF MITER = 4, CALL RES, THEN JAC, AND MULTIPLY BY SCALAR. ------------
 400  IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NRE = NRE + 1 
      IF (IRES .GT. 1) GO TO 600
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      LENP = MEBAND*N
      DO 410 I = 1,LENP
 410    WM(I+2) = 0.0D0
      CALL JAC ( NEQ, TN, Y, S, ML, MU, WM(ML3), MEBAND)
      CON = -HL0
      DO 420 I = 1,LENP
 420    WM(I+2) = WM(I+2)*CON 
      GO TO 570
C IF MITER = 5, MAKE ML + MU + 2 CALLS TO RES TO APPROXIMATE J. --------
 500  CONTINUE
      IRES = -1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NRE = NRE + 1 
      IF (IRES .GT. 1) GO TO 600
      ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MBA = MIN0(MBAND,N)
      MEBAND = MBAND + ML
      MEB1 = MEBAND - 1
      SRUR = WM(1)
      DO 560 J = 1,MBA
        DO 530 I = J,N,MBAND
          YI = Y(I) 
          R = DMAX1(SRUR*DABS(YI),0.01D0/EWT(I))
 530      Y(I) = Y(I) + R
        CALL RES ( NEQ, TN, Y, S, RTEM, IRES)
        NRE = NRE + 1
        IF (IRES .GT. 1) GO TO 600
        DO 550 JJ = J,N,MBAND 
          Y(JJ) = YH(JJ,1)
          YJJ = Y(JJ)
          R = DMAX1(SRUR*DABS(YJJ),0.01D0/EWT(JJ))
          FAC = -HL0/R
          I1 = MAX0(JJ-MU,1)
          I2 = MIN0(JJ+ML,N)
          II = JJ*MEB1 - ML + 2
          DO 540 I = I1,I2
 540        WM(II+I) = (RTEM(I) - SAVR(I))*FAC
 550      CONTINUE
 560    CONTINUE
      IRES = 1
      CALL RES (NEQ, TN, Y, S, SAVR, IRES)
      NRE = NRE + 1 
      IF (IRES .GT. 1) GO TO 600
C ADD MATRIX A. --------------------------------------------------------
 570  CONTINUE
      CALL ADDA(NEQ, TN, Y, ML, MU, WM(ML3), MEBAND)
C DO LU DECOMPOSITION OF P. --------------------------------------------
      CALL DGBFA (WM(3), MEBAND, N, ML, MU, IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C ERROR RETURN FOR IRES = 2 OR IRES = 3 RETURN FROM RES. ---------------
 600  IERPJ = IRES
      RETURN
C----------------------- END OF SUBROUTINE PREPJI ----------------------
      END 
