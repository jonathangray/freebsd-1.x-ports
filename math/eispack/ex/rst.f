C
C     THIS INPUT SUBROUTINE READS A REAL SYMMETRIC BAND MATRIX
C     FROM SYSIN OF ORDER  N,  AND BAND WIDTH  MB .
C     TO GENERATE THE MATRIX  ST  INITIALLY,  INITIL  IS TO BE 0.
C     TO REGENERATE THE MATRIX  ST  FOR THE PURPOSE OF THE RESIDUAL
C     CALCULATION,  INITIL  IS TO BE  1.
C
C     THIS ROUTINE IS CATALOGUED AS  EISPDRV4(RSBREADI).
C
      DOUBLE PRECISION ST(20,20),STHOLD(20,20)
      INTEGER  IA( 5)
      DATA IREADA/1/,IWRITE/6/
C
      open(unit=ireada,file='FILE51')
      rewind ireada
      write(6,*)' what matrix do you want?'
      read(5,*)nn
    1 continue
      READ(IREADA,5) N,MB
    5 FORMAT(2I6)
      IF( N .EQ. 0 )  GO TO  70
      DO 8 I = 1,N
         DO 7 J = 1,MB
            ST(I,J) = 0.0D0
    7    CONTINUE
    8 CONTINUE
      DO 15 I=1,N
         MBB = MIN0(MB,N-I+1)
         READ(IREADA,10) (IA(J),J=1,MBB)
   10    FORMAT(6I12)
         DO 11 J=1,MBB
           M = MB+1-J
           K = I+J-1
   11      ST(K,M) = DFLOAT(IA(J))
   15 CONTINUE
   19 continue
      if( nn .ne. n )  go to 1
      do 21 i = 1,n
         st(i,i)=st(i,2)
         if( i .ne. 2 ) st(i,2) = 0.0
   21 continue
      st(1,2) = st(2,1)
      do 23 i = 3,n
         st(i,i-1) = st(i,1)
         st(i-1,i) = st(i,i-1)
         st(i,1) = 0.0
   23 continue
      write(6,*)'a=<'
      do 20 i = 1,n
         write(6,22)(st(i,j),j=1,n)
   22    format(10f8.0)
   20 continue
      write(6,*)'>'
      go to 1
   99 continue
   70 continue
      STOP
      END
