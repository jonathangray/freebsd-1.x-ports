      SUBROUTINE SOLBT (M, N, A, B, C, Y, IP)
      INTEGER M, N, IP(M,N)
      DOUBLE PRECISION A(M,M,N), B(M,M,N), C(M,M,N), Y(M,N) 
CLLL. OPTIMIZE
C-----------------------------------------------------------------------
C SOLUTION OF BLOCK-TRIDIAGONAL LINEAR SYSTEM.
C COEFFICIENT MATRIX MUST HAVE BEEN PREVIOUSLY PROCESSED BY DECBT.
C M, N, A, B, C, AND IP  MUST NOT HAVE BEEN CHANGED SINCE CALL TO DECBT.
C WRITTEN BY A. C. HINDMARSH. 
C INPUT.. 
C     M = ORDER OF EACH BLOCK.
C     N = NUMBER OF BLOCKS IN EACH DIRECTION OF MATRIX.
C A,B,C = M BY M BY N ARRAYS CONTAINING BLOCK LU DECOMPOSITION
C         OF COEFFICIENT MATRIX FROM DECBT.
C    IP = M BY N INTEGER ARRAY OF PIVOT INFORMATION FROM DECBT.
C     Y = ARRAY OF LENGTH M*N CONTAING THE RIGHT-HAND SIDE VECTOR
C         (TREATED AS AN M BY N ARRAY HERE).
C OUTPUT..
C     Y = SOLUTION VECTOR, OF LENGTH M*N.
C
C EXTERNAL ROUTINES REQUIRED.. DGESL (LINPACK) AND DDOT (BLAS).
C-----------------------------------------------------------------------
C
      INTEGER NM1, NM2, I, K, KB, KM1, KP1
      DOUBLE PRECISION DP, DDOT
      NM1 = N - 1
      NM2 = N - 2
C FORWARD SOLUTION SWEEP. ----------------------------------------------
      CALL DGESL (A, M, M, IP, Y, 0)
      DO 30 K = 2,NM1
        KM1 = K - 1 
        DO 20 I = 1,M
          DP = DDOT (M, C(I,1,K), M, Y(1,KM1), 1) 
          Y(I,K) = Y(I,K) - DP
 20       CONTINUE
        CALL DGESL (A(1,1,K), M, M, IP(1,K), Y(1,K), 0)
 30     CONTINUE
      DO 50 I = 1,M 
        DP = DDOT (M, C(I,1,N), M, Y(1,NM1), 1)
     1     + DDOT (M, B(I,1,N), M, Y(1,NM2), 1)
        Y(I,N) = Y(I,N) - DP
 50     CONTINUE
      CALL DGESL (A(1,1,N), M, M, IP(1,N), Y(1,N), 0)
C BACKWARD SOLUTION SWEEP. ---------------------------------------------
      DO 80 KB = 1,NM1
        K = N - KB
        KP1 = K + 1 
        DO 70 I = 1,M
          DP = DDOT (M, B(I,1,K), M, Y(1,KP1), 1) 
          Y(I,K) = Y(I,K) - DP
 70       CONTINUE
 80     CONTINUE
      DO 100 I = 1,M
        DP = DDOT (M, C(I,1,1), M, Y(1,3), 1)
        Y(I,1) = Y(I,1) - DP
 100    CONTINUE
      RETURN
C-----------------------  END OF SUBROUTINE SOLBT  ---------------------
      END 
