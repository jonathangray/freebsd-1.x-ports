      SUBROUTINE MDM
     *     (VK,TAIL, V,L, LAST,NEXT, MARK)
CLLL. OPTIMIZE
C***********************************************************************
C  MDM -- FORM ELEMENT FROM UNELIMINATED NEIGHBORS OF VK
C***********************************************************************
      INTEGER  VK, TAIL,  V(1), L(1),   LAST(1), NEXT(1),   MARK(1),
     *   TAG, S,LS,VS,ES, B,LB,VB, BLP,BLPMAX
      EQUIVALENCE  (VS, ES)
C
C----INITIALIZE TAG AND LIST OF UNELIMINATED NEIGHBORS
      TAG = MARK(VK)
      TAIL = VK
C
C----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK
      LS = L(VK)
   1  S = LS
      IF (S.EQ.0)  GO TO 5
        LS = L(S)
        VS = V(S)
        IF (NEXT(VS).LT.0)  GO TO 2
C
C------IF VS IS UNELIMINATED VERTEX, THEN TAG AND APPEND TO LIST OF
C------UNELIMINATED NEIGHBORS 
          MARK(VS) = TAG
          L(TAIL) = S
          TAIL = S
          GO TO 4
C
C------IF ES IS ACTIVE ELEMENT, THEN ...
C--------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES
   2      LB = L(ES)
          BLPMAX = LAST(ES)
          DO 3 BLP=1,BLPMAX
            B = LB
            LB = L(B)
            VB = V(B)
C
C----------IF VB IS UNTAGGED VERTEX, THEN TAG AND APPEND TO LIST OF
C----------UNELIMINATED NEIGHBORS
            IF (MARK(VB).GE.TAG)  GO TO 3
              MARK(VB) = TAG
              L(TAIL) = B
              TAIL = B
   3        CONTINUE
C
C--------MARK ES INACTIVE
          MARK(ES) = TAG
C
   4    GO TO 1
C
C----TERMINATE LIST OF UNELIMINATED NEIGHBORS
   5  L(TAIL) = 0
C
      RETURN
      END 
