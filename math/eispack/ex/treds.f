      SUBROUTINE tred1(NM,N,A,D,E,E2)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),E2(N)
      REAL c,F,G,H,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX
C     TO A SYMMETRIC TRIDIAGONAL MATRIX USING
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS-
C          FORMATIONS USED IN THE REDUCTION IN ITS STRICT LOWER
C          TRIANGLE.  THE FULL UPPER TRIANGLE OF A IS UNALTERED.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
  100 D(I) = A(n,I)
      do 110 i = 1, n
  110 a(n,i) = a(i,i)
C     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 1) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(d(k))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = 0.0E0
         E2(I) = 0.0E0
         if (l .lt. 1) go to 300
         do 135 k = 1, l
            f = d(k)
            d(k) = a(l,k)
            a(l,k) = a(i,k)
            a(i,k) = f
  135    continue 
         go to 300
C
  140    DO 150 K = 1, L
            d(k) = d(k) / SCALE
            H = H + d(k) * d(k)
  150    CONTINUE
C
         E2(I) = SCALE * SCALE * H
         f = d(l)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         d(l) = f - g
         if (l .gt. 1) go to 160
         f = d(1)
         d(1) = a(1,1)
         a(1,1) = a(2,1)
         a(2,1) = f * scale
         go to 300
  160    continue
         F = 0.0E0
C
         do 170 k = 1, l
  170    e(k) = 0.0e0
         DO 240 J = 1, L
            c = d(j)
            g = e(j) + a(j,j) * c
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
               e(k) = e(k) + a(k,j)*c
               g = g + a(k,j) * d(k)
  200       continue
C     .......... FORM ELEMENT OF P ..........
  220       E(J) = G / H
            f = f + e(j) * c
  240    CONTINUE
C
         H = F / (H + H)
C     .......... FORM REDUCED A ..........
         do 250 k = 1, l
  250    e(k) = e(k) - h * d(k)          
         do 280 j = 1, l
            f = d(j)
            g = e(j)
C
            do 260 k = j, l
  260       a(k,j) = a(k,j) - g * d(k) - f * e(k)
            d(j) = a(l,j)
            a(l,j) = a(i,j)
            a(i,j) = f * scale
  280    continue
C
  300 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRED2(NM,N,A,D,E,Z)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),Z(NM,N)
      REAL c,F,G,H,HH,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED2,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX TO A
C     SYMMETRIC TRIDIAGONAL MATRIX USING AND ACCUMULATING
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
C
C        Z CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX
C          PRODUCED IN THE REDUCTION.
C
C        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
C
         DO 80 J = i, n
   80    z(j,i) = a(j,i)
         d(i) = a(n,i)
  100 CONTINUE
C
      IF (N .EQ. 1) GO TO 320
C     .......... FOR I=N STEP -1 UNTIL 2 DO -- ..........
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 2) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(d(k))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = d(l)
         h = 0.0e0
         do 135 j = 1,l
            z(i,j) = 0.0e0
            z(j,i) = d(j)
            d(j) = z(l,j)
  135    continue
         GO TO 290
C
  140    DO 150 K = 1, L
            d(k) = d(k) / SCALE
            H = H + d(k) * d(k)
  150    CONTINUE
C
         F = d(l)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         d(l) = F - G
         F = 0.0E0
C
         do 170 k = 1, l
  170    e(k) = 0.0e0           
         DO 240 J = 1, L
            c = d(j)
            z(j,i) = c
            g = e(j) + z(j,j)*c
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
               e(k) = e(k) + z(k,j) * c
               G = G + Z(K,J) * d(k)
  200       continue
C     .......... FORM ELEMENT OF P ..........
  220       E(J) = G / H
            F = F + E(J) * c
  240    CONTINUE
C
         HH = F / (H + H)
C     .......... FORM REDUCED A ..........
         do 250 k = 1, l
  250    e(k) = e(k) - hh * d(k)          
         DO 280 J = 1, L
            F = d(j)
            G = E(J)
C
            DO 260 K = j, l
  260       Z(k,j) = Z(k,j) - g * d(k) - f * e(k)
            d(j) = z(l,j)
            z(i,j) = 0.0e0
  280    continue
C
  290    D(I) = H
  300 CONTINUE
C
  320 e(1) = 0.0E0
C     .......... ACCUMULATION OF TRANSFORMATION MATRICES ..........
      if (n .eq. 1) go to 510
      DO 500 I = 2, N
         L = I - 1
         z(n,l) = z(l,l) 
         z(l,l) = 1.0e0
         h = d(i)
         IF (h .EQ. 0.0E0) GO TO 380
C
         do 330 k = 1,l
  330    d(k) = z(k,i) / h         
         DO 360 J = 1, L
            G = 0.0E0
C
            DO 340 K = 1, L
  340       G = G + Z(k,i) * Z(K,J)
C
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * d(k)
  360    CONTINUE
C
  380    do 400 k = 1, l
  400    Z(k,I) = 0.0E0
C
  500 CONTINUE
C
  510 do 520 i = 1,n
         d(i) = z(n,i)
         z(n,i) = 0.0e0
  520 continue
      z(n,n) = 1.0e0
c
      RETURN
      END


