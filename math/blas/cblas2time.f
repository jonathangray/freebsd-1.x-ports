      PROGRAM CB2TIM
*
***********************************************************************
*
*  Timing program for the COMPLEX          Level 2 BLAS.
*
*  The program calls a REAL function SECOND with no arguments, which
*  is assumed to return the central-processor time in seconds from
*  some fixed starting-time.
*
*  The program must be driven by a short data file, which specifies
*  values for the matrix dimensions M and N, the bandwidth K, and
*  for the leading array dimension LDA. The data file is read from
*  unit NIN. Results are written to unit NOUT. NIN and NOUT are set
*  to 5 and 6 respectively in a PARAMETER statement, but can easily
*  be changed.
*
*  The first 9 records of the data file are read using list-directed
*  input, the last 16 records are read using the format ( A ). An
*  annotated example of a data file can be obtained by deleting the
*  first 3 characters from the following 28 lines:
*  Data file for timing program for the COMPLEX          Level 2 BLAS
*  5                     Number of values of M (maximum 12)
*  32 64 128 256 512     The values of M
*  5                     Number of values of N (maximum 12)
*  32 64 128 256 512     The values of N
*  2                     Number of values of K (maximum 12)
*  8 32                  The values of K
*  2                     Number of values of INCX = INCY (maximum 4)
*  1 2                   The values of INCX = INCY
*  2                     Number of values of LDA (maximum 12)
*  512 513               The values of LDA
*  CGEMV     T           ('T' or 't' means 'Time this routine')
*  CGBMV     T
*  CHEMV     T
*  CHBMV     T
*  CHPMV     T
*  CTRMV     T
*  CTBMV     T
*  CTPMV     T
*  CTRSV     T
*  CTBSV     T
*  CTPSV     T
*  CGERU     T
*  CGERC     T
*  CHER      T
*  CHPR      T
*  CHER2     T
*  CHPR2     T
*
*  The maximum value for M and N (NMAX) is defined in a PARAMETER
*  statement as 512. The maximum value for INCX and INCY (INCMAX) is
*  defined in a PARAMETER statement as 32. The maximum value of LDA
*  is defined in a PARAMETER statement as NMAX+32. All three can
*  easily be changed. The maximum value for K is (NMAX-1)/2.
*
*  Each routine is timed for all possible values of the options
*  TRANS and UPLO. Option DIAG is always set to 'N'. All elements
*  of the matrices are initialised to non-zero values. ALPHA and
*  BETA are set to values other than 0, 1, -1. The routines are
*  timed for all combinations of applicable values of M, N, K, LDA
*  and INCX=INCY. Note that this can take a considerable amount of
*  time. It is important that the Level 2 BLAS should perform as well
*  as possible for all shapes and sizes of matrices.
*
***********************************************************************
*
*  -- Written on 28-August-1989.
*     Jeremy Du Croz and Mick Pont, NAG Central Office.
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            MAXVAL, MXNINC, MXNLDA
      PARAMETER          ( MAXVAL = 12, MXNINC = 4, MXNLDA = 12 )
      INTEGER            NMAX, KMAX, INCMAX, LDAMAX
      PARAMETER          ( NMAX = 512, KMAX = ( NMAX - 1 )/2,
     $                   INCMAX = 32, LDAMAX = NMAX + 32 )
      INTEGER            LA, LX
      PARAMETER          ( LA = NMAX*LDAMAX, LX = NMAX*INCMAX )
      INTEGER            NTRANS, NUPLOS
      PARAMETER          ( NTRANS = 3, NUPLOS = 2 )
      INTEGER            LDRES
      PARAMETER          ( LDRES = MAXVAL )
      INTEGER            NSUBS
      PARAMETER          ( NSUBS = 17 )
*     .. Local Scalars ..
      INTEGER            I, IINC, ILDA, INCX, ISUB, ITA, IUPLO, J, LDA,
     $                   MATCH, NINCS, NKVALS, NLDA, NMVALS, NNVALS
      LOGICAL            IXANDY
      CHARACTER*80       LINE
*     .. Local Arrays ..
      COMPLEX            A( LA ), X( LX ), Y( LX )
      REAL               RESLTS( LDRES, MAXVAL, MXNLDA, MXNINC )
      INTEGER            INCS( MXNINC ), KVALS( MAXVAL ),
     $                   LDAVAL( MXNLDA ), MVALS( MAXVAL ),
     $                   NVALS( MAXVAL )
      LOGICAL            TSTSUB( NSUBS )
      CHARACTER*1        TRANS( NTRANS ), UPLOS( NUPLOS )
      CHARACTER*6        NAMES( NSUBS )
*     .. External Subroutines ..
      EXTERNAL           CTIM01, CTIM02, CTIM03, CTIM04, CTIM05, CTIM06,
     $                   CTIM07, CTIM08, CTIM09, CTIM10, CTIM11, CTIM12,
     $                   CTIM13, CTIM14, CTIM15, CTIM16, CTIM17, PRNTAB,
     $                   PRNTB1
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     .. Data statements ..
      DATA               NAMES/'CGEMV ', 'CGBMV ', 'CHEMV ', 'CHBMV ',
     $                   'CHPMV ', 'CTRMV ', 'CTBMV ', 'CTPMV ',
     $                   'CTRSV ', 'CTBSV ', 'CTPSV ', 'CGERU ',
     $                   'CGERC ', 'CHER  ', 'CHPR  ', 'CHER2 ',
     $                   'CHPR2 '/
      DATA               TRANS/'N', 'T', 'C'/
      DATA               UPLOS/'U', 'L'/
*     .. Executable Statements ..
*
*     Read driving file.
*
      READ( NIN, FMT = * )
      READ( NIN, FMT = * )NMVALS
      IF( NMVALS.GT.MAXVAL )THEN
         WRITE( NOUT, FMT = * )'Too many values of M: NMVALS = ', NMVALS
         STOP
      END IF
      READ( NIN, FMT = * )( MVALS( I ), I = 1, NMVALS )
      DO 10 I = 1, NMVALS
         IF( MVALS( I ).GT.NMAX )THEN
            WRITE( NOUT, FMT = * )'A value of M is too large: value is '
     $         , MVALS( I )
            STOP
         END IF
   10 CONTINUE
      READ( NIN, FMT = * )NNVALS
      IF( NNVALS.GT.MAXVAL )THEN
         WRITE( NOUT, FMT = * )'Too many values of N: NNVALS = ', NNVALS
         STOP
      END IF
      READ( NIN, FMT = * )( NVALS( I ), I = 1, NNVALS )
      DO 20 I = 1, NNVALS
         IF( NVALS( I ).GT.NMAX )THEN
            WRITE( NOUT, FMT = * )'A value of N is too large: value is '
     $         , NVALS( I )
            STOP
         END IF
   20 CONTINUE
      READ( NIN, FMT = * )NKVALS
      IF( NKVALS.GT.MAXVAL )THEN
         WRITE( NOUT, FMT = * )'Too many values of K: NKVALS = ', NKVALS
         STOP
      END IF
      READ( NIN, FMT = * )( KVALS( I ), I = 1, NKVALS )
      DO 30 I = 1, NKVALS
         IF( KVALS( I ).GT.KMAX )THEN
            WRITE( NOUT, FMT = * )'A value of K is too large: value is '
     $         , KVALS( I )
            STOP
         END IF
   30 CONTINUE
      READ( NIN, FMT = * )NINCS
      IF( NINCS.GT.MXNINC )THEN
         WRITE( NOUT, FMT = * )'Too many values of INCX: NINCS = ',
     $      NINCS
         STOP
      END IF
      READ( NIN, FMT = * )( INCS( I ), I = 1, NINCS )
      DO 40 I = 1, NINCS
         IF( INCS( I ).GT.INCMAX )THEN
            WRITE( NOUT, FMT = * )
     $         'A value of INCX is too large: value is ', INCS( I )
            STOP
         END IF
   40 CONTINUE
      READ( NIN, FMT = * )NLDA
      IF( NLDA.GT.MXNLDA )THEN
         WRITE( NOUT, FMT = * )'Too many values of LDA: NLDA = ', NLDA
         STOP
      END IF
      READ( NIN, FMT = * )( LDAVAL( I ), I = 1, NLDA )
      DO 50 I = 1, NLDA
         IF( LDAVAL( I ).GT.LDAMAX )THEN
            WRITE( NOUT, FMT = * )
     $         'A value of LDA is too large: value is ', LDAVAL( I )
            STOP
         END IF
   50 CONTINUE
*
*     Decide which routines are to be timed.
*
   60 READ( NIN, FMT = '(A)', END = 90 )LINE
      MATCH = 0
      DO 70 I = 1, NSUBS
         IF( NAMES( I ).EQ.LINE( 1: 6 ) )
     $      MATCH = I
   70 CONTINUE
      IF( MATCH.EQ.0 )THEN
         WRITE( NOUT, FMT = * )'Unknown subroutine name: ''',
     $      LINE( 1: 6 ), ''''
         STOP
      END IF
      I = 7
   80 IF( LINE( I: I ).EQ.' '.AND.I.LT.80 )THEN
         I = I + 1
         GO TO 80
      END IF
*
*     We will time routine if first non-blank character is 'T' or 't'.
*
      TSTSUB( MATCH ) = LINE( I: I ).EQ.'T'.OR.LINE( I: I ).EQ.'t'
      GO TO 60
   90 CONTINUE
*
*     Time each routine
*
      DO 650 ISUB = 1, NSUBS
         IF( TSTSUB( ISUB ) )THEN
*
*           Generate data
*
            DO 100 I = 1, LA
               A( I ) = CMPLX( 1.0E0 + REAL( I )/( LA + 1 ),
     $                  -1.0E0 + REAL( I )/( LA + 1 ) )
  100       CONTINUE
            DO 110 I = 1, LX
               X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
               Y( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
  110       CONTINUE
*
*           Print header.
*
            WRITE( NOUT, FMT = 9999 )NAMES( ISUB )
            IXANDY = ISUB.LE.5.OR.ISUB.EQ.12.OR.ISUB.EQ.13.OR.ISUB.EQ.
     $               16.OR.ISUB.EQ.17
            IF( NAMES( ISUB )( 3: 3 ).NE.'P' )THEN
               IF( NLDA*NINCS.EQ.1 )THEN
                  IF( IXANDY )THEN
                     WRITE( NOUT, FMT = 9998 )LDAVAL( 1 ), INCS( 1 )
                  ELSE
                     WRITE( NOUT, FMT = 9997 )LDAVAL( 1 ), INCS( 1 )
                  END IF
               ELSE
                  DO 130 I = 1, NLDA
                     DO 120 J = 1, NINCS
                        IF( IXANDY )THEN
                           WRITE( NOUT, FMT = 9994 )( I - 1 )*NINCS + J,
     $                        LDAVAL( I ), INCS( J )
                        ELSE
                           WRITE( NOUT, FMT = 9993 )( I - 1 )*NINCS + J,
     $                        LDAVAL( I ), INCS( J )
                        END IF
  120                CONTINUE
  130             CONTINUE
               END IF
            ELSE
               IF( NINCS.EQ.1 )THEN
                  IF( IXANDY )THEN
                     WRITE( NOUT, FMT = 9996 )INCS( 1 )
                  ELSE
                     WRITE( NOUT, FMT = 9995 )INCS( 1 )
                  END IF
               ELSE
                  DO 140 J = 1, NINCS
                     IF( IXANDY )THEN
                        WRITE( NOUT, FMT = 9992 )J, INCS( J )
                     ELSE
                        WRITE( NOUT, FMT = 9991 )J, INCS( J )
                     END IF
  140             CONTINUE
               END IF
            END IF
*
            IF( NAMES( ISUB ).EQ.'CGEMV ' )THEN
               DO 170 ITA = 1, NTRANS
                  DO 160 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 150 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM01( TRANS( ITA ), NMAX, MVALS, NMVALS,
     $                               NVALS, NNVALS, A, LDA, INCX, X, Y,
     $                               LDRES, RESLTS( 1, 1, ILDA, IINC ) )
  150                CONTINUE
  160             CONTINUE
                  WRITE( NOUT, FMT = 9990 )TRANS( ITA )
                  CALL PRNTAB( 'M', 'N', MVALS, NMVALS, NVALS, NNVALS,
     $                         NINCS, NLDA, RESLTS, LDRES, MAXVAL,
     $                         MAXVAL, NOUT )
  170          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CGBMV ' )THEN
               DO 200 ITA = 1, NTRANS
                  DO 190 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 180 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM02( TRANS( ITA ), NMAX, KVALS, NKVALS,
     $                               NVALS, NNVALS, A, LDA, INCX, X, Y,
     $                               LDRES, RESLTS( 1, 1, ILDA, IINC ) )
  180                CONTINUE
  190             CONTINUE
                  WRITE( NOUT, FMT = 9989 )TRANS( ITA )
                  CALL PRNTAB( 'K', 'N', KVALS, NKVALS, NVALS, NNVALS,
     $                         NINCS, NLDA, RESLTS, LDRES, MAXVAL,
     $                         MAXVAL, NOUT )
  200          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CHEMV ' )THEN
               DO 230 IUPLO = 1, NUPLOS
                  DO 220 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 210 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM03( UPLOS( IUPLO ), NMAX, NVALS,
     $                               NNVALS, A, LDA, INCX, X, Y, LDRES,
     $                               RESLTS( 1, 1, ILDA, IINC ) )
  210                CONTINUE
  220             CONTINUE
                  WRITE( NOUT, FMT = 9988 )UPLOS( IUPLO )
                  CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, NLDA, RESLTS,
     $                         LDRES, MAXVAL, MAXVAL, NOUT )
  230          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CHBMV ' )THEN
               DO 260 IUPLO = 1, NUPLOS
                  DO 250 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 240 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM04( UPLOS( IUPLO ), NMAX, KVALS,
     $                               NKVALS, NVALS, NNVALS, A, LDA,
     $                               INCX, X, Y, LDRES,
     $                               RESLTS( 1, 1, ILDA, IINC ) )
  240                CONTINUE
  250             CONTINUE
                  WRITE( NOUT, FMT = 9987 )UPLOS( IUPLO )
                  CALL PRNTAB( 'K', 'N', KVALS, NKVALS, NVALS, NNVALS,
     $                         NINCS, NLDA, RESLTS, LDRES, MAXVAL,
     $                         MAXVAL, NOUT )
  260          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CHPMV ' )THEN
               DO 280 IUPLO = 1, NUPLOS
                  ILDA = 1
                  LDA = LDAVAL( ILDA )
                  DO 270 IINC = 1, NINCS
                     INCX = INCS( IINC )
                     CALL CTIM05( UPLOS( IUPLO ), NMAX, NVALS, NNVALS,
     $                            A, LDA, INCX, X, Y, LDRES,
     $                            RESLTS( 1, 1, ILDA, IINC ) )
  270             CONTINUE
                  WRITE( NOUT, FMT = 9986 )UPLOS( IUPLO )
                  CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, 1, RESLTS,
     $                         LDRES, MAXVAL, MAXVAL, NOUT )
  280          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CTRMV ' )THEN
               DO 320 IUPLO = 1, NUPLOS
                  DO 310 ITA = 1, NTRANS
                     DO 300 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 290 IINC = 1, NINCS
                           INCX = INCS( IINC )
                           CALL CTIM06( UPLOS( IUPLO ), TRANS( ITA ),
     $                                  NMAX, NVALS, NNVALS, A, LDA,
     $                                  INCX, X, LDRES,
     $                                  RESLTS( 1, 1, ILDA, IINC ) )
  290                   CONTINUE
  300                CONTINUE
                     WRITE( NOUT, FMT = 9985 )UPLOS( IUPLO ),
     $                  TRANS( ITA )
                     CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, NLDA,
     $                            RESLTS, LDRES, MAXVAL, MAXVAL, NOUT )
  310             CONTINUE
  320          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CTBMV ' )THEN
               DO 360 IUPLO = 1, NUPLOS
                  DO 350 ITA = 1, NTRANS
                     DO 340 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 330 IINC = 1, NINCS
                           INCX = INCS( IINC )
                           CALL CTIM07( UPLOS( IUPLO ), TRANS( ITA ),
     $                                  NMAX, KVALS, NKVALS, NVALS,
     $                                  NNVALS, A, LDA, INCX, X, LDRES,
     $                                  RESLTS( 1, 1, ILDA, IINC ) )
  330                   CONTINUE
  340                CONTINUE
                     WRITE( NOUT, FMT = 9984 )UPLOS( IUPLO ),
     $                  TRANS( ITA )
                     CALL PRNTAB( 'K', 'N', KVALS, NKVALS, NVALS,
     $                            NNVALS, NINCS, NLDA, RESLTS, LDRES,
     $                            MAXVAL, MAXVAL, NOUT )
  350             CONTINUE
  360          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CTPMV ' )THEN
               DO 390 IUPLO = 1, NUPLOS
                  DO 380 ITA = 1, NTRANS
                     ILDA = 1
                     LDA = LDAVAL( ILDA )
                     DO 370 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM08( UPLOS( IUPLO ), TRANS( ITA ), NMAX,
     $                               NVALS, NNVALS, A, LDA, INCX, X,
     $                               LDRES, RESLTS( 1, 1, ILDA, IINC ) )
  370                CONTINUE
                     WRITE( NOUT, FMT = 9983 )UPLOS( IUPLO ),
     $                  TRANS( ITA )
                     CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, 1, RESLTS,
     $                            LDRES, MAXVAL, MAXVAL, NOUT )
  380             CONTINUE
  390          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CTRSV ' )THEN
               DO 430 IUPLO = 1, NUPLOS
                  DO 420 ITA = 1, NTRANS
                     DO 410 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 400 IINC = 1, NINCS
                           INCX = INCS( IINC )
                           CALL CTIM09( UPLOS( IUPLO ), TRANS( ITA ),
     $                                  NMAX, NVALS, NNVALS, A, LDA,
     $                                  INCX, X, LDRES,
     $                                  RESLTS( 1, 1, ILDA, IINC ) )
  400                   CONTINUE
  410                CONTINUE
                     WRITE( NOUT, FMT = 9982 )UPLOS( IUPLO ),
     $                  TRANS( ITA )
                     CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, NLDA,
     $                            RESLTS, LDRES, MAXVAL, MAXVAL, NOUT )
  420             CONTINUE
  430          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CTBSV ' )THEN
               DO 470 IUPLO = 1, NUPLOS
                  DO 460 ITA = 1, NTRANS
                     DO 450 ILDA = 1, NLDA
                        LDA = LDAVAL( ILDA )
                        DO 440 IINC = 1, NINCS
                           INCX = INCS( IINC )
                           CALL CTIM10( UPLOS( IUPLO ), TRANS( ITA ),
     $                                  NMAX, KVALS, NKVALS, NVALS,
     $                                  NNVALS, A, LDA, INCX, X, LDRES,
     $                                  RESLTS( 1, 1, ILDA, IINC ) )
  440                   CONTINUE
  450                CONTINUE
                     WRITE( NOUT, FMT = 9981 )UPLOS( IUPLO ),
     $                  TRANS( ITA )
                     CALL PRNTAB( 'K', 'N', KVALS, NKVALS, NVALS,
     $                            NNVALS, NINCS, NLDA, RESLTS, LDRES,
     $                            MAXVAL, MAXVAL, NOUT )
  460             CONTINUE
  470          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CTPSV ' )THEN
               DO 500 IUPLO = 1, NUPLOS
                  DO 490 ITA = 1, NTRANS
                     ILDA = 1
                     LDA = LDAVAL( ILDA )
                     DO 480 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM11( UPLOS( IUPLO ), TRANS( ITA ), NMAX,
     $                               NVALS, NNVALS, A, LDA, INCX, X,
     $                               LDRES, RESLTS( 1, 1, ILDA, IINC ) )
  480                CONTINUE
                     WRITE( NOUT, FMT = 9980 )UPLOS( IUPLO ),
     $                  TRANS( ITA )
                     CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, 1, RESLTS,
     $                            LDRES, MAXVAL, MAXVAL, NOUT )
  490             CONTINUE
  500          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CGERU  ' )THEN
               DO 520 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 510 IINC = 1, NINCS
                     INCX = INCS( IINC )
                     CALL CTIM12( NMAX, MVALS, NMVALS, NVALS, NNVALS, A,
     $                            LDA, INCX, X, Y, LDRES,
     $                            RESLTS( 1, 1, ILDA, IINC ) )
  510             CONTINUE
  520          CONTINUE
               WRITE( NOUT, FMT = 9979 )
               CALL PRNTAB( 'M', 'N', MVALS, NMVALS, NVALS, NNVALS,
     $                      NINCS, NLDA, RESLTS, LDRES, MAXVAL, MAXVAL,
     $                      NOUT )
            ELSE IF( NAMES( ISUB ).EQ.'CGERC  ' )THEN
               DO 540 ILDA = 1, NLDA
                  LDA = LDAVAL( ILDA )
                  DO 530 IINC = 1, NINCS
                     INCX = INCS( IINC )
                     CALL CTIM13( NMAX, MVALS, NMVALS, NVALS, NNVALS, A,
     $                            LDA, INCX, X, Y, LDRES,
     $                            RESLTS( 1, 1, ILDA, IINC ) )
  530             CONTINUE
  540          CONTINUE
               WRITE( NOUT, FMT = 9978 )
               CALL PRNTAB( 'M', 'N', MVALS, NMVALS, NVALS, NNVALS,
     $                      NINCS, NLDA, RESLTS, LDRES, MAXVAL, MAXVAL,
     $                      NOUT )
            ELSE IF( NAMES( ISUB ).EQ.'CHER  ' )THEN
               DO 570 IUPLO = 1, NUPLOS
                  DO 560 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 550 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM14( UPLOS( IUPLO ), NMAX, NVALS,
     $                               NNVALS, A, LDA, INCX, X, LDRES,
     $                               RESLTS( 1, 1, ILDA, IINC ) )
  550                CONTINUE
  560             CONTINUE
                  WRITE( NOUT, FMT = 9977 )UPLOS( IUPLO )
                  CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, NLDA, RESLTS,
     $                         LDRES, MAXVAL, MAXVAL, NOUT )
  570          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CHPR  ' )THEN
               DO 590 IUPLO = 1, NUPLOS
                  ILDA = 1
                  LDA = LDAVAL( ILDA )
                  DO 580 IINC = 1, NINCS
                     INCX = INCS( IINC )
                     CALL CTIM15( UPLOS( IUPLO ), NMAX, NVALS, NNVALS,
     $                            A, LDA, INCX, X, LDRES,
     $                            RESLTS( 1, 1, ILDA, IINC ) )
  580             CONTINUE
                  WRITE( NOUT, FMT = 9976 )UPLOS( IUPLO )
                  CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, 1, RESLTS,
     $                         LDRES, MAXVAL, MAXVAL, NOUT )
  590          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CHER2 ' )THEN
               DO 620 IUPLO = 1, NUPLOS
                  DO 610 ILDA = 1, NLDA
                     LDA = LDAVAL( ILDA )
                     DO 600 IINC = 1, NINCS
                        INCX = INCS( IINC )
                        CALL CTIM16( UPLOS( IUPLO ), NMAX, NVALS,
     $                               NNVALS, A, LDA, INCX, X, Y, LDRES,
     $                               RESLTS( 1, 1, ILDA, IINC ) )
  600                CONTINUE
  610             CONTINUE
                  WRITE( NOUT, FMT = 9975 )UPLOS( IUPLO )
                  CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, NLDA, RESLTS,
     $                         LDRES, MAXVAL, MAXVAL, NOUT )
  620          CONTINUE
            ELSE IF( NAMES( ISUB ).EQ.'CHPR2 ' )THEN
               DO 640 IUPLO = 1, NUPLOS
                  ILDA = 1
                  LDA = LDAVAL( ILDA )
                  DO 630 IINC = 1, NINCS
                     INCX = INCS( IINC )
                     CALL CTIM17( UPLOS( IUPLO ), NMAX, NVALS, NNVALS,
     $                            A, LDA, INCX, X, Y, LDRES,
     $                            RESLTS( 1, 1, ILDA, IINC ) )
  630             CONTINUE
                  WRITE( NOUT, FMT = 9974 )UPLOS( IUPLO )
                  CALL PRNTB1( 'N', NVALS, NNVALS, NINCS, 1, RESLTS,
     $                         LDRES, MAXVAL, MAXVAL, NOUT )
  640          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9973 )
         END IF
  650 CONTINUE
*
 9999 FORMAT( '1*** Speed of ', A, ' in megaflops ***' )
 9998 FORMAT( 5X, 'with LDA = ', I5, ' and INCX = INCY = ', I5 )
 9997 FORMAT( 5X, 'with LDA = ', I5, ' and INCX = ', I5 )
 9996 FORMAT( 5X, 'with INCX = INCY = ', I5 )
 9995 FORMAT( 5X, 'with INCX = ', I5 )
 9994 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5,
     $      ' and INCX = INCY = ', I5 )
 9993 FORMAT( 5X, 'line ', I2, ' with LDA = ', I5, ' and INCX = ', I5 )
 9992 FORMAT( 5X, 'line ', I2, ' with INCX = INCY = ', I5 )
 9991 FORMAT( 5X, 'line ', I2, ' with INCX = ', I5 )
 9990 FORMAT( /1X, 'CGEMV with TRANS = ''', A, '''', / )
 9989 FORMAT( /1X, 'CGBMV with TRANS = ''', A, ''', M = N and KL = KU ',
     $      ' = K', / )
 9988 FORMAT( /1X, 'CHEMV with UPLO = ''', A, '''', / )
 9987 FORMAT( /1X, 'CHBMV with UPLO = ''', A, '''', / )
 9986 FORMAT( /1X, 'CHPMV with UPLO = ''', A, '''', / )
 9985 FORMAT( /1X, 'CTRMV with UPLO = ''', A, ''', TRANS = ''', A, '''',
     $      / )
 9984 FORMAT( /1X, 'CTBMV with UPLO = ''', A, ''', TRANS = ''', A, '''',
     $      / )
 9983 FORMAT( /1X, 'CTPMV with UPLO = ''', A, ''', TRANS = ''', A, '''',
     $      / )
 9982 FORMAT( /1X, 'CTRSV with UPLO = ''', A, ''', TRANS = ''', A, '''',
     $      / )
 9981 FORMAT( /1X, 'CTBSV with UPLO = ''', A, ''', TRANS = ''', A, '''',
     $      / )
 9980 FORMAT( /1X, 'CTPSV with UPLO = ''', A, ''', TRANS = ''', A, '''',
     $      / )
 9979 FORMAT( /1X, 'CGERU', / )
 9978 FORMAT( /1X, 'CGERC', / )
 9977 FORMAT( /1X, 'CHER with UPLO = ''', A, '''', / )
 9976 FORMAT( /1X, 'CHPR with UPLO = ''', A, '''', / )
 9975 FORMAT( /1X, 'CHER2 with UPLO = ''', A, '''', / )
 9974 FORMAT( /1X, 'CHPR2 with UPLO = ''', A, '''', / )
 9973 FORMAT( ///// )
      END
*
      SUBROUTINE PRNTAB( LAB1, LAB2, M, LENM, N, LENN, NINCS, NLDA,
     $                   RESLTS, LDRES, SECRES, THDRES, NOUT )
*     .. Scalar Arguments ..
      INTEGER            LDRES, LENM, LENN, NINCS, NLDA, NOUT, SECRES,
     $                   THDRES
      CHARACTER*1        LAB1, LAB2
*     .. Array Arguments ..
      REAL               RESLTS( LDRES, SECRES, THDRES, NINCS )
      INTEGER            M( LENM ), N( LENN )
*     .. Local Scalars ..
      INTEGER            I, J, K, L
*     .. Executable Statements ..
      WRITE( NOUT, FMT = 9999 )LAB2, ( N( I ), I = 1, LENN )
      WRITE( NOUT, FMT = 9998 )LAB1
      DO 30 I = 1, LENM
         WRITE( NOUT, FMT = 9997 )M( I ),
     $      ( RESLTS( I, J, 1, 1 ), J = 1, LENN )
         DO 20 K = 1, NLDA
            DO 10 L = 1, NINCS
               IF( K*L.GT.1 )
     $            WRITE( NOUT, FMT = 9996 )( RESLTS( I, J, K, L ),
     $            J = 1, LENN )
   10       CONTINUE
   20    CONTINUE
         IF( NLDA*NINCS.GT.1 )
     $      WRITE( NOUT, FMT = * )
   30 CONTINUE
      RETURN
*
 9999 FORMAT( 7X, A, I6, 11I9 )
 9998 FORMAT( 4X, A )
 9997 FORMAT( 1X, I4, 12F9.2 )
 9996 FORMAT( 5X, 12F9.2 )
      END
*
      SUBROUTINE PRNTB1( LAB1, N, LENN, NINCS, NLDA, RESLTS, LDRES,
     $                   SECRES, THDRES, NOUT )
*     .. Scalar Arguments ..
      INTEGER            LDRES, LENN, NINCS, NLDA, NOUT, SECRES, THDRES
      CHARACTER*1        LAB1
*     .. Array Arguments ..
      REAL               RESLTS( LDRES, SECRES, THDRES, NINCS )
      INTEGER            N( LENN )
*     .. Local Scalars ..
      INTEGER            I, J, K, L
*     .. Executable Statements ..
      WRITE( NOUT, FMT = 9999 )LAB1, ( N( I ), I = 1, LENN )
      WRITE( NOUT, FMT = * )
      DO 20 K = 1, NLDA
         DO 10 L = 1, NINCS
            WRITE( NOUT, FMT = 9998 )( RESLTS( 1, J, K, L ), J = 1,
     $         LENN )
   10    CONTINUE
   20 CONTINUE
      WRITE( NOUT, FMT = * )
      RETURN
*
 9999 FORMAT( 7X, A, I6, 11I9 )
 9998 FORMAT( 5X, 12F9.2 )
      END
*
      SUBROUTINE CTIM01( TRANS, NMAX, MVALS, NMVALS, NVALS, NNVALS, A,
     $                   LDA, INCX, X, Y, LDRES, RESLTS )
*     Times CGEMV.
*     .. Parameters ..
      COMPLEX            ALPHA, BETA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ),
     $                   BETA = ( 1.1E0, -0.3E0 ) )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NMVALS, NNVALS
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            MVALS( NMVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IMVAL, INVAL, M, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CGEMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 30 IMVAL = 1, NMVALS
         M = MVALS( IMVAL )
         DO 20 INVAL = 1, NNVALS
            N = NVALS( INVAL )
*           Re-initialise Y each time to avoid overflow.
            DO 10 I = 1, N*INCX
               Y( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CGEMV( TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y,
     $                  INCX )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            NOP = 8*M*N + 6*( M + N )
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IMVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IMVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM02( TRANS, NMAX, KVALS, NKVALS, NVALS, NNVALS, A,
     $                   LDA, INCX, X, Y, LDRES, RESLTS )
*     Times CGBMV.
*     .. Parameters ..
      COMPLEX            ALPHA, BETA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ),
     $                   BETA = ( 1.1E0, -0.3E0 ) )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NKVALS, NMAX, NNVALS
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            KVALS( NKVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IKVAL, INVAL, K, K1, M, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CGBMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MIN
*     .. Executable Statements ..
      DO 30 IKVAL = 1, NKVALS
         K = KVALS( IKVAL )
         DO 20 INVAL = 1, NNVALS
            N = NVALS( INVAL )
            M = N
*           Re-initialise Y each time to avoid overflow.
            DO 10 I = 1, N*INCX
               Y( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CGBMV( TRANS, M, N, K, K, ALPHA, A, LDA, X, INCX, BETA,
     $                  Y, INCX )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            K1 = MIN( K, N )
            NOP = 8*( ( 2*K1 + 1 )*N - K1*( K1 + 1 ) ) + 12*N
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IKVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IKVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM03( UPLO, NMAX, NVALS, NNVALS, A, LDA, INCX, X, Y,
     $                   LDRES, RESLTS )
*     Times CHEMV.
*     .. Parameters ..
      COMPLEX            ALPHA, BETA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ),
     $                   BETA = ( 1.1E0, -0.3E0 ) )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHEMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 20 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise Y each time to avoid overflow.
         DO 10 I = 1, N*INCX
            Y( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CHEMV( UPLO, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCX )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 8*N*N + 8*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM04( UPLO, NMAX, KVALS, NKVALS, NVALS, NNVALS, A,
     $                   LDA, INCX, X, Y, LDRES, RESLTS )
*     Times CHBMV.
*     .. Parameters ..
      COMPLEX            ALPHA, BETA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ),
     $                   BETA = ( 1.1E0, -0.3E0 ) )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            INCX, LDA, LDRES, NKVALS, NMAX, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            KVALS( NKVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IKVAL, INVAL, K, K1, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHBMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MIN
*     .. Executable Statements ..
      DO 30 IKVAL = 1, NKVALS
         K = KVALS( IKVAL )
         DO 20 INVAL = 1, NNVALS
            N = NVALS( INVAL )
*           Re-initialise Y each time to avoid overflow.
            DO 10 I = 1, N*INCX
               Y( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CHBMV( UPLO, N, K, ALPHA, A, LDA, X, INCX, BETA, Y,
     $                  INCX )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            K1 = MIN( K, N )
            NOP = 8*( ( 2*K1 + 1 )*N - K1*( K1 + 1 ) ) + 8*N
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IKVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IKVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM05( UPLO, NMAX, NVALS, NNVALS, A, LDA, INCX, X, Y,
     $                   LDRES, RESLTS )
*     Times CHPMV.
*     .. Parameters ..
      COMPLEX            ALPHA, BETA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ),
     $                   BETA = ( 1.1E0, -0.3E0 ) )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHPMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 20 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise Y each time to avoid overflow.
         DO 10 I = 1, N*INCX
            Y( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CHPMV( UPLO, N, ALPHA, A, X, INCX, BETA, Y, INCX )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 8*N*N + 8*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM06( UPLO, TRANS, NMAX, NVALS, NNVALS, A, LDA, INCX,
     $                   X, LDRES, RESLTS )
*     Times CTRMV.
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
      CHARACTER*1        TRANS, UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CTRMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 20 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise X each time to avoid overflow.
         DO 10 I = 1, N*INCX
            X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CTRMV( UPLO, TRANS, 'N', N, A, LDA, X, INCX )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 4*N*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM07( UPLO, TRANS, NMAX, KVALS, NKVALS, NVALS,
     $                   NNVALS, A, LDA, INCX, X, LDRES, RESLTS )
*     Times CTBMV.
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NKVALS, NMAX, NNVALS
      CHARACTER*1        TRANS, UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            KVALS( NKVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IKVAL, INVAL, K, K1, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CTBMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MIN
*     .. Executable Statements ..
      DO 30 IKVAL = 1, NKVALS
         K = KVALS( IKVAL )
         DO 20 INVAL = 1, NNVALS
            N = NVALS( INVAL )
*           Re-initialise X each time to avoid overflow.
            DO 10 I = 1, N*INCX
               X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CTBMV( UPLO, TRANS, 'N', N, K, A, LDA, X, INCX )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            K1 = MIN( K, N )
            NOP = 4*( 2*N*K1 - K1*( K1 + 1 ) ) + 4*N
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IKVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IKVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM08( UPLO, TRANS, NMAX, NVALS, NNVALS, A, LDA, INCX,
     $                   X, LDRES, RESLTS )
*     Times CTPMV.
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
      CHARACTER*1        TRANS, UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CTPMV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 20 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise X each time to avoid overflow.
         DO 10 I = 1, N*INCX
            X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CTPMV( UPLO, TRANS, 'N', N, A, X, INCX )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 4*N*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM09( UPLO, TRANS, NMAX, NVALS, NNVALS, A, LDA, INCX,
     $                   X, LDRES, RESLTS )
*     Times CTRSV.
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
      CHARACTER*1        TRANS, UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CTRSV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 20 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise X each time to avoid overflow.
         DO 10 I = 1, N*INCX
            X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CTRSV( UPLO, TRANS, 'N', N, A, LDA, X, INCX )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 4*N*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM10( UPLO, TRANS, NMAX, KVALS, NKVALS, NVALS,
     $                   NNVALS, A, LDA, INCX, X, LDRES, RESLTS )
*     Times CTBSV.
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NKVALS, NMAX, NNVALS
      CHARACTER*1        TRANS, UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            KVALS( NKVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IKVAL, INVAL, K, K1, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CTBSV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MIN
*     .. Executable Statements ..
      DO 30 IKVAL = 1, NKVALS
         K = KVALS( IKVAL )
         DO 20 INVAL = 1, NNVALS
            N = NVALS( INVAL )
*           Re-initialise X each time to avoid overflow.
            DO 10 I = 1, N*INCX
               X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CTBSV( UPLO, TRANS, 'N', N, K, A, LDA, X, INCX )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            K1 = MIN( K, N )
            NOP = 4*( 2*N*K1 - K1*( K1 + 1 ) ) + 4*N
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IKVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IKVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM11( UPLO, TRANS, NMAX, NVALS, NNVALS, A, LDA, INCX,
     $                   X, LDRES, RESLTS )
*     Times CTPSV.
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
      CHARACTER*1        TRANS, UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CTPSV
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX
*     .. Executable Statements ..
      DO 20 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise X each time to avoid overflow.
         DO 10 I = 1, N*INCX
            X( I ) = CMPLX( I*1.013E0, -I*0.987E0 )
   10    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CTPSV( UPLO, TRANS, 'N', N, A, X, INCX )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 4*N*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   20 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM12( NMAX, MVALS, NMVALS, NVALS, NNVALS, A, LDA,
     $                   INCX, X, Y, LDRES, RESLTS )
*     Times CGERU.
*     .. Parameters ..
      COMPLEX            ALPHA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ) )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NMVALS, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            MVALS( NMVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IMVAL, INVAL, J, M, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CGERU
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MIN, REAL
*     .. Executable Statements ..
      DO 40 IMVAL = 1, NMVALS
         M = MVALS( IMVAL )
         DO 30 INVAL = 1, NNVALS
            N = NVALS( INVAL )
*           Re-initialise A each time to avoid overflow.
            DO 20 J = 1, N
               DO 10 I = 1, M
                  A( I, J ) = CMPLX( 1.0E0 + REAL( ( J - 1 )*N + I )/
     $                        ( M*N + 1 ), -1.0E0 +
     $                        REAL( ( J - 1 )*N + I )/( M*N + 1 ) )
   10          CONTINUE
   20       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CGERU( M, N, ALPHA, X, INCX, Y, INCX, A, LDA )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            NOP = 6*( M*N + MIN( M, N ) ) + 2*M*N
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IMVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IMVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM13( NMAX, MVALS, NMVALS, NVALS, NNVALS, A, LDA,
     $                   INCX, X, Y, LDRES, RESLTS )
*     Times CGERC.
*     .. Parameters ..
      COMPLEX            ALPHA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ) )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NMVALS, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            MVALS( NMVALS ), NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, IMVAL, INVAL, J, M, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CGERC
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, MIN, REAL
*     .. Executable Statements ..
      DO 40 IMVAL = 1, NMVALS
         M = MVALS( IMVAL )
         DO 30 INVAL = 1, NNVALS
            N = NVALS( INVAL )
*           Re-initialise A each time to avoid overflow.
            DO 20 J = 1, N
               DO 10 I = 1, M
                  A( I, J ) = CMPLX( 1.0E0 + REAL( ( J - 1 )*N + I )/
     $                        ( M*N + 1 ), -1.0E0 +
     $                        REAL( ( J - 1 )*N + I )/( M*N + 1 ) )
   10          CONTINUE
   20       CONTINUE
            S0 = SECOND( )
            S1 = SECOND( )
            CALL CGERC( M, N, ALPHA, X, INCX, Y, INCX, A, LDA )
            S2 = SECOND( )
            TIME = ( S2 - S1 ) - ( S1 - S0 )
            NOP = 6*( M*N + MIN( M, N ) ) + 2*M*N
            IF( TIME.LE.0.0E0 )THEN
               RESLTS( IMVAL, INVAL ) = 0.0E0
            ELSE
               RESLTS( IMVAL, INVAL ) = NOP/( 1.0E6*TIME )
            END IF
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM14( UPLO, NMAX, NVALS, NNVALS, A, LDA, INCX, X,
     $                   LDRES, RESLTS )
*     Times CHER.
*     .. Parameters ..
      REAL               ALPHA
      PARAMETER          ( ALPHA = -0.9E0 )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
      CHARACTER*1        UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, J, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHER
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     .. Executable Statements ..
      DO 30 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise A each time to avoid overflow.
         DO 20 J = 1, N
            DO 10 I = 1, N
               A( I, J ) = CMPLX( 1.0E0 + REAL( ( J - 1 )*N + I )/
     $                     ( N*N + 1 ), -1.0E0 +
     $                     REAL( ( J - 1 )*N + I )/( N*N + 1 ) )
   10       CONTINUE
   20    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CHER( UPLO, N, ALPHA, X, INCX, A, LDA )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 4*N*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM15( UPLO, NMAX, NVALS, NNVALS, A, LDA, INCX, X,
     $                   LDRES, RESLTS )
*     Times CHPR.
*     .. Parameters ..
      REAL               ALPHA
      PARAMETER          ( ALPHA = -0.9E0 )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
      CHARACTER*1        UPLO
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, J, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHPR
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     .. Executable Statements ..
      DO 30 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise A each time to avoid overflow.
         DO 20 J = 1, N
            DO 10 I = 1, N
               A( I, J ) = CMPLX( 1.0E0 + REAL( ( J - 1 )*N + I )/
     $                     ( N*N + 1 ), -1.0E0 +
     $                     REAL( ( J - 1 )*N + I )/( N*N + 1 ) )
   10       CONTINUE
   20    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CHPR( UPLO, N, ALPHA, X, INCX, A )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 4*N*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM16( UPLO, NMAX, NVALS, NNVALS, A, LDA, INCX, X, Y,
     $                   LDRES, RESLTS )
*     Times CHER2.
*     .. Parameters ..
      COMPLEX            ALPHA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ) )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, J, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHER2
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     .. Executable Statements ..
      DO 30 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise A each time to avoid overflow.
         DO 20 J = 1, N
            DO 10 I = 1, N
               A( I, J ) = CMPLX( 1.0E0 + REAL( ( J - 1 )*N + I )/
     $                     ( N*N + 1 ), -1.0E0 +
     $                     REAL( ( J - 1 )*N + I )/( N*N + 1 ) )
   10       CONTINUE
   20    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CHER2( UPLO, N, ALPHA, X, INCX, Y, INCX, A, LDA )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 8*( N + 1 )*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   30 CONTINUE
      RETURN
      END
*
      SUBROUTINE CTIM17( UPLO, NMAX, NVALS, NNVALS, A, LDA, INCX, X, Y,
     $                   LDRES, RESLTS )
*     Times CHPR2.
*     .. Parameters ..
      COMPLEX            ALPHA
      PARAMETER          ( ALPHA = ( -0.9E0, 0.7E0 ) )
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            INCX, LDA, LDRES, NMAX, NNVALS
*     .. Array Arguments ..
      COMPLEX            A( LDA, NMAX ), X( NMAX*INCX ),
     $                   Y( NMAX*INCX )
      REAL               RESLTS( LDRES, NNVALS )
      INTEGER            NVALS( NNVALS )
*     .. Local Scalars ..
      REAL               S0, S1, S2, TIME
      INTEGER            I, INVAL, J, N, NOP
*     .. External Functions ..
      REAL               SECOND
      EXTERNAL           SECOND
*     .. External Subroutines ..
      EXTERNAL           CHPR2
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     .. Executable Statements ..
      DO 30 INVAL = 1, NNVALS
         N = NVALS( INVAL )
*           Re-initialise A each time to avoid overflow.
         DO 20 J = 1, N
            DO 10 I = 1, N
               A( I, J ) = CMPLX( 1.0E0 + REAL( ( J - 1 )*N + I )/
     $                     ( N*N + 1 ), -1.0E0 +
     $                     REAL( ( J - 1 )*N + I )/( N*N + 1 ) )
   10       CONTINUE
   20    CONTINUE
         S0 = SECOND( )
         S1 = SECOND( )
         CALL CHPR2( UPLO, N, ALPHA, X, INCX, Y, INCX, A )
         S2 = SECOND( )
         TIME = ( S2 - S1 ) - ( S1 - S0 )
         NOP = 8*( N + 1 )*N
         IF( TIME.LE.0.0E0 )THEN
            RESLTS( 1, INVAL ) = 0.0E0
         ELSE
            RESLTS( 1, INVAL ) = NOP/( 1.0E6*TIME )
         END IF
   30 CONTINUE
      RETURN
      END
